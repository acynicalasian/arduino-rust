/*
    My own library to deal with reading and writing from flash on-system. I may extend this library
    to include IO involving the USART port or IO operations from the SPI (serial programming
    interface).

    Important note: flash memory is WORD-ADDRESSED rather than BYTE-ADDRESSED.

    Big warning: we have no memory management built into our "bootloader"/"OS", not to mention that
    because we're in a `no-std` environment, there's no concept of a heap whatsoever. I doubt that
    we'll run into memory issues, but to alleviate some potential pain, I've hard capped the size
    of the arrays we use to back up flash memory pages and to hold the bytes we read from flash.
    Unfortunately, we seemingly have to return raw pointers, so be *VERY* aware that I can't stop
    whoever uses this code from accessing memory before `READBUF` or past the `n` bytes we read and
    copied into `READBUF`. As for reserving a big chunk of memory (roughly 38% of RAM?), hopefully
    on-board flash memory management is important enough that this memory reservation is justified.

    There honestly *might* be a HAL provided by arduino_hal or one of its related crates for flash
    storage management but it's so heavily abstracted that I'm having trouble even understanding
    what's going on in those libraries, especially given that I'm just hacking together Rust code
    and m328p assembly.
*/

use panic_halt as _;
use core::arch::asm;

/*
    We probably can't write past 0x3EFF in flash because of the lock bits, which prevent SPM
    instructions from operating on the boot section of flash memory, not to mention this is also
    undesirable behavior for us since we want to make it easier to flash the Arduino.
*/
pub const MAX_MEM_ADDR: usize = 0x3EFF;

/*
    As mentioned earlier, we'll explicitly reserve 128 bytes of RAM for reading flash memory.
    This will get overwritten with every new read we do to memory!

    Assuming you read `n` bytes where `n` < 128, reading past `n` bytes in `READBUF` will never
    fail because READBUF has a

    Reading past `n` bytes leads to undefined behavior.

    We can separate the max size for `READBUF` into a `const` here so editing it is easier in the
    future. Just be aware that once we start writing linker scripts to include this library in our
    hacky bootloader, changing this value directly changes the bootloader size.
*/
pub const MAX_READBUF_SIZE: usize = 256;
#[used]
static mut READBUF: [u8; MAX_READBUF_SIZE] = [0; MAX_READBUF_SIZE];

/*
    Rust makes it insanely annoying to just treat a 16 bit integer as two bytes, so we'll reserve
    2 bytes via static allocation as a 16-bit and then tell Rust we're actually pointing to a byte.
*/
#[used]
static mut BACKUP_U16: usize = 0;

/*
    Read and return the high byte of the word in flash memory at address `adr`.
*/
#[inline(always)]
pub unsafe fn read_highbyte(adr: usize) -> u8 {
    return _read_byte(adr, 1);
}

/*
    Read and return the low byte of the word in flash memory at address `adr`.
*/
#[inline(always)]
pub unsafe fn read_lowbyte(adr: usize) -> u8 {
    return _read_byte(adr, 0);
}

/*
    Read `n` bytes starting at flash memory address `adr` and return a raw pointer to `READBUF`.
    Apparently, I can't just return an immutable reference to `READBUF`, so unfortunately, I have
    no way to guarantee that we can't access memory before `READBUF` or past the `n` bytes we read.
    Be very, very aware!!
*/
#[inline(always)]
pub unsafe fn read_nbytes(adr: usize, n: usize) -> *const u8 {
    _read_nbytes(adr, n, 0);
    return &READBUF[0];
}

/*
    Utility function in case we REALLY need the granularity of reading `n` bytes starting from
    the high byte of the word at `adr`.
*/
#[inline(always)]
pub unsafe fn read_nbytes_starthigh(adr: usize, n: usize) -> *const u8 {
    _read_nbytes(adr, n, 1);
    return &READBUF[0];
}

/*
    Syntactic sugar for if we REALLY want to make it explicit that we're reading `n` bytes starting
    from the low byte of a word.
*/
#[inline(always)]
pub unsafe fn read_nbytes_startlow(adr: usize, n: usize) -> *const u8 {
    return read_nbytes(adr, n);
}

/*
    Write `input` byte to the high byte of the word in flash memory at address `adr`.
    Realistically, this and its low byte version are utility functions for the library; the main
    use of the library would be to copy one half of our application memory to the other half of
    application memory when implementing our A/B firmware upgrade system.

    Note that `adr` is the "raw" memory address that'd be used by LPM instructions as well. The
    memory addressing system for SPM instructions is confusing as heck so we'll calculate the
    appropriate address internally.
 */
#[inline(always)]
pub unsafe fn write_highbyte(adr: usize, input: u8) {
    return _write_byte(adr, 1, input);
}

/*
    Low byte equivalent to above.
*/
#[inline(always)]
pub unsafe fn write_lowbyte(adr: usize, input: u8) {
    return _write_byte(adr, 0, input);
}

#[inline(always)]
unsafe fn _read_byte(adr: usize, offset: u8) -> u8 {
    _check_adr(adr);
    let res: u8;
    unsafe {
        _lpm_setup_regs(adr, offset);
        // Load our target byte into the register used by `res` and then restore our Z register.
        asm!(
            "lpm {res}, Z", res = out(reg) res,
        );
        _lpm_restore_regs();
    }
    return res;
}

/*
    Read `n` bytes from flash memory starting from address `adr`
*/
unsafe fn _read_nbytes(adr: usize, n: usize, offset: u8) {
    _check_adr(adr);
    _check_numbytes(n);
    unsafe {
        _lpm_setup_regs(adr, offset);
    }
    for i in 0..n as usize {
        let b: u8;
        // Use LPM with post-increment because we can take advantage of how the Z register value is
        // structured... if we read the low byte at an address, we need to read the high byte next,
        // and the lowest bit in the Z register controls this! If we read the high byte at an
        // address, we want to read the next memory address starting from the low byte. We know the
        // lowest bit is 1... incrementing sets this to 0, and we add the carry to the next bit,
        // which is the lowest significant bit of the target memory address.
        unsafe {
            asm!(
                "lpm {b}, Z+", b = out(reg) b,
            );
            READBUF[i] = b;
        }
    }
    unsafe {
        _lpm_restore_regs();
    }
}

unsafe fn _write_byte(adr: usize, offset: u8, input: u8) {
    _check_adr(adr);
    unsafe {
        return;
    }
}

/*
    Set up the necessary registers for an LPM instruction by calculating the proper value for the Z
    register and backing up the Z register before actually running the LPM instruction.
*/
#[inline(always)]
unsafe fn _lpm_setup_regs(adr: usize, offset: u8) {
    // LPM uses our target address, bit shifts it left once (multiplies by 2), and uses the lowest
    // bit (our offset) to determine whether we're reading the high or low byte of the 2-byte word
    // at `adr`. Hopefully our compiler knows to store `zreg_value` in two consecutive registers
    // because we're going to use a format string with an MOVW instruction to copy our calculuated
    // `zreg_value` into the Z register.
    let zreg_value = (adr*2) + offset as usize;
    unsafe {
        // By setting our static mut `BACKUP_U16` equal to `zreg_value`, we can probably guarantee
        // that in our compiled code, we copy over `zreg_value` contiguously to the location of
        // `BACKUP_U16` in SRAM such that we can now treat `BACKUP_U16` as two contiguous bytes in
        // memory. This avoids our compiler complaining about a MOVW instruction that operates
        // on `zreg_value` without casting it to a `u8` (which I still have no clue how that'd
        // behave... like, would we just lose the upper byte if we cast?).
        BACKUP_U16 = zreg_value;
        let backup_adr = &raw const BACKUP_U16 as usize;
        let zreg_lowbyte = *(backup_adr as *const u8);
        let zreg_highbyte = *((backup_adr + 1usize) as *const u8);
        asm!(
            "cli",              // Disable interrupts
            "push r30",         // Back up Z-register, used for LPM instructions
            "push r31",
            "mov r30, {zl}",    // Set r30 to zreg_lowbyte and r31 to zreg_highbyte.
            "mov r31, {zh}",
            zl = in(reg) zreg_lowbyte,
            zh = in(reg) zreg_highbyte,
            // We DON'T WANT TO SET 
        );
    }
}

/*
    Restore the registers we used for an LPM instruction and reenable interrupts to avoid the side
    effects of using this code.
*/
#[inline(always)]
unsafe fn _lpm_restore_regs() {
    unsafe {
        asm!(
            "pop r31",
            "pop r30",
            "sei",
        );
    }
}

/*
    Addresses in the same page as `adr` have the same upper 10 bits. Back up the memory page
    corresponding to `adr` and fill the temporary page buffer.
*/
#[inline(always)]
unsafe fn _fill_pagebuf(adr: usize) {
    _check_adr(adr);
    let mut adr_lowerbound = adr & 0x03FC0; // Corresponds to zeroing out the lower 6 bits of `adr`.
    // Use LPM instructions to read from adr_lowerbound onwards to fill the temporary page buffer.
    unsafe {
        _lpm_setup_regs(adr_lowerbound, 0);
        // We can't just reuse our code from `read_nbytes()` because filling the temporary buffer
        // also requires us to use r0 and r1 to store each word we read from flash memory.
        asm!(
            "push r0",          // Back up these two registers for use in filling the temp pagebuf.
            "push r1",
            "push r2",          // We'll use r2 to store the previous value of SPMCSR.

        );
        // We're moving 64 words.
        for _ in 0..64 {
            asm!(
                // Copy low bits to r0 and high bits to r1.
                "lpm r0, Z+",
                "lpm r1, Z+",
                
            );
        }
    }
}

#[inline(always)]
fn _check_adr(adr: usize) {
    if adr > MAX_MEM_ADDR {
        // Halt on failure.
        panic!("Attempted to access invalid flash memory address!");
    }
}
#[inline(always)]
fn _check_numbytes(n: usize) {
    if n > MAX_READBUF_SIZE {
        // Halt on failure.
        panic!("Attempted to read more than 256 bytes at a time!");
    } else if n < 1 {
        panic!("Attempted to read zero bytes!");
    }
}