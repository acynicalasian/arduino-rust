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
    From what I can glean online, there is no dedicated page buffer when writing to flash memory
    one page at a time. That means we need to reserve 128 bytes (64 words) of SRAM if we write to
    flash.
*/
pub const MAX_PAGEBUF_SIZE: usize = 128;    // DO NOT CHANGE THIS VALUE!!
#[used]
static mut PAGEBUF: [u8; MAX_PAGEBUF_SIZE] = [0; MAX_PAGEBUF_SIZE];

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
        asm!(
            "cli",              // Disable interrupts
            "push r30",         // Back up Z-register, used for LPM instructions
            "push r31",
            "movw r30, {z}",    // Load the Z-register with the formatted address for LPM.
            z = in(reg) zreg_value as u8,   // Hopefully this cast doesn't trigger an internal MOV
                                            // instruction that separates the low and high bytes of
                                            // `zreg_value`!
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