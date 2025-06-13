/*
    My own library to deal with reading and writing from flash on-system. I may extend this library
    to include IO involving the USART port or IO operations from the SPI (serial programming
    interface).

    Important note: flash memory is WORD-ADDRESSED rather than BYTE-ADDRESSED.

    Big warning: we have no memory management built into our "bootloader"/"OS". As I understand it,
    NONE of Rust's memory safety guarantees apply if we do things in `unsafe` blocks (which is
    basically most of our code at this point considering how low-level it is) so I simply can't
    guarantee that if I return a pointer to some arbitrary value in SRAM when we read `n` bytes
    flash memory, the SRAM pointer will still be valid and we won't have overwritten any of that
    memory. I mean, we also just have no concept of a heap whatsoever in `no-std` environments. To
    try to make things safer, I've restricted reading flash memory to 256 bytes at a time and have
    explicitly reserved 256 bytes of RAM. I hate using so much RAM here, but flash IO is important
    enough that I think I can justify reserving this amount.

    There honestly *might* be a HAL provided by arduino_hal or one of its related crates for flash
    storage management but it's so heavily abstracted that I'm having trouble even understanding
    what's going on in those libraries, especially given that I'm just hacking together Rust code
    and m328p assembly.
*/

use panic_halt as _;
use core::arch::asm;

const MAX_MEM_ADDR: u16 = 0x3EFF;       // We probably can't write past 0x3EFF in flash because of
                                        // the lock bits, which prevent SPM instructions from
                                        // operationg on the boot section of flash memory, not to
                                        // mention this is also undesirable behavior for us since
                                        // we want to make it easier to flash the Arduino.
const RZ: u8 = 0x1E;
const RZ_LOW: u8 = RZ;
const RZ_HIGH: u8 = RZ + 1;
const R16: u8 = 0x10;

/*
    The static keyword here *should* reserve a byte of RAM that will remain valid throughout the
    lifetime of my program. That means that I won't ever have to worry about which registers the
    compiler decides to use... worrying that I unintentionally overwrote the register being used
    to hold a value read from flash before returning the value read won't be a problem anymore.
 */
#[used]
static mut READBYTE: u8 = 0;

/*
    From what I can glean online, there is no dedicated page buffer when writing to flash memory
    one page at a time. That means we need to reserve 128 bytes (64 words) of SRAM if we write to
    flash.
*/
#[used]
static mut PAGEBUF: [u8; 128] = [0; 128];

/*
    As mentioned earlier, we'll explicitly reserve 256 bytes of RAM for reading flash memory.
    This will get overwritten with every new read we do to memory!

    Assuming you read `n` bytes where `n` < 256, reading past `n` bytes in `READBUF` will never
    fail but will result in undefined behavior. You'll either read zeroes (we haven't read more
    than `n` bytes yet) or get data from a previous read operation that's no longer valid.
*/
#[used]
static mut READBUF: [u8; 256] = [0; 256];

/*
    Reserve some memory where we can back up the arguments passed to these functions; because the
    compiler treats inline assembly as a black box, I have no clue whether I can guarantee that
    there will never be register collisions even if I back up the values of registers I know my
    functions will use.
    
    i.e.
        READBUF[i] = *(R16 as *const u8);

    `READBUF[i]` should lead to a 16-bit value... what if we're using r16 to store the address of
    `READBUF`? Or, what if r16 was used to hold the number of bytes to read/write or the offset?
    This type of confusion is very undesirable so we'll back up these values since static values
    have a location in SRAM that'll never change.
 */
#[used]
static mut ADR_BACKUP: u16 = 0;
#[used]
static mut INPUTBYTE_BACKUP: u8 = 0;
#[used]
static mut INPUTPTR_BACKUP: u16 = 0;
#[used]
static mut NUMBYTES_BACKUP: u8 = 0;
#[used]
static mut OFFSET_BACKUP: u8 = 0;

/*
    Read and return the high byte of the word in flash memory at address `adr`.
 */
#[inline(always)]
pub unsafe fn read_highbyte(adr: u16) -> u8 {
    return _read_byte(adr, 1);
}

/*
    Read and return the low byte of the word in flash memory at address `adr`.
 */
#[inline(always)]
pub unsafe fn read_lowbyte(adr: u16) -> u8 {
    return _read_byte(adr, 0);
}

/*
    Read `n` bytes starting at flash memory address `adr` and return a pointer to `READBUF`. We
    (hopefully) can't modify `READBUF` outside of this library, so we can return a `const` raw
    pointer. This function always starts at the low byte of the word.
*/
#[inline(always)]
pub unsafe fn read_nbytes(adr: u16, n: u8) -> *const u8 {
    _read_nbytes(adr, n, 0);
    return &READBUF[0];
}

/*
    Utility function in case we REALLY need the granularity of reading `n` bytes starting from
    the high byte of the word at `adr`.
*/
#[inline(always)]
pub unsafe fn read_nbytes_starthigh(adr: u16, n: u8) -> *const u8 {
    _read_nbytes(adr, n, 1);
    return &READBUF[0];
}

/*
    Syntactic sugar for if we REALLY want to make it explicit that we're reading `n` bytes starting
    from the low byte of a word.
*/
#[inline(always)]
pub unsafe fn read_nbytes_startlow(adr: u16, n: u8) -> *const u8 {
    return read_nbytes(adr, n);
}

/*
    Write `input` byte to the high byte of the word in flash memory at address `adr`.
    Realistically, this and its low byte version are utility functions for the library; the main
    use of the library would be to copy one half of our application memory to the other half of
    application memory when implementing our A/B firmware upgrade system.
 */
#[inline(always)]
pub unsafe fn write_highbyte(adr: u16, input: u8) {
    return write_byte(adr, 1, input);
}

/*
    Low byte equivalent to above.
*/
#[inline(always)]
pub unsafe fn write_lowbyte(adr: u16, input: u8) {
    return write_byte(adr, 0, input);
}

#[inline(always)]
unsafe fn _read_byte(adr: u16, offset: u8) -> u8 {
    _check_adr(adr);
    unsafe {
        _lpm_backup_regs();
        _lpm_setup_zreg(adr, offset);
        // Load our target byte into r16 and then into READBYTE.
        asm!(
            "lpm r16, Z",
        );
        READBYTE = *(R16 as *const u8);
        _lpm_restore_regs();
    }
    return READBYTE;
}

unsafe fn _read_nbytes(adr: u16, n: u8, offset: u8) {
    _check_adr(adr);
    unsafe {
        _lpm_backup_regs();
        _lpm_setup_zreg(adr, offset);
    }
    for i in 0..n as usize {
        // Load our target byte into r16 and then into READBUF. We don't need to back up r16 and
        // the Z register every time we read a byte. We'll just back things up at the end.

    }
}

unsafe fn write_byte(adr: u16, offset: u8, input: u8) {
    _check_adr(adr);
    unsafe {
        return;
    }
}

#[inline(always)]
fn _check_adr(adr: u16) {
    if adr > MAX_MEM_ADDR {
        // Halt on failure.
        panic!("Attempted to access invalid flash memory address!");
    }
}

/*
    Set up the necessary registers for an LPM instruction.
*/
#[inline(always)]
unsafe fn _lpm_backup_regs() {
    unsafe {
        asm!(
            "cli",              // Disable interrupts
            "push r28",         // Back up Y-register
            "push r29",
            "push r30",         // Back up Z-register
            "push r31",
            "push r16",         // Back up r16, we'll use it to hold the byte we read
        );
    }
}

/*
    Restore the registers we used for an LPM instruction to avoid the side effects of using this
    code... Our compiler can't check inline assembly code so we need to be able to guarantee that
    our code didn't mess up register values if we perform RW operations in the middle of code that
    we didn't write or can't guarantee the behavior of.
*/
#[inline(always)]
unsafe fn _lpm_restore_regs() {
    unsafe {
        // Fix our registers and reenable interrupts... we only wanted to read one byte.
        asm!(
            "pop r16",
            "pop r31",
            "pop r30",
            "pop r29",
            "pop r28",
            "sei",
        );
    }
}

/*
    Setup our Z-register with the correct address for an LPM instruction.
*/
#[inline(always)]
unsafe fn _lpm_setup_zreg(adr: u16, offset: u8) {
    unsafe {
        // Set the Z-register to our target address. `offset` determines whether we took the high
        // or low byte of the word at `adr`.
        // This code feels insanely hacky since we're operating on two different registers like
        // they're memory, but I don't see how this wouldn't work. It's a lot more readable than
        // taking the "sure" approach of calculating the offset address and assigning the high
        // byte to r31 and the low byte to r30. Hopefully the compiler is smart enough to tell that
        // I want to put the low byte of `offset_adr` into r30 and the high byte into r31.
        let offset_adr = adr*2;
        *(RZ as *mut u16) = offset_adr;
        // This hacky cast helps ensure that I can pass `offset` as a u8, which makes sure I only
        // use one register. Extremely nitpicky but it could matter? Otherwise, I have to do
        // `*(RZ as *mut u16) = offset_adr + (offset as u16)` because the linter complains about
        // type incompatibility between u16 and u8. And then at that point, I have to wonder if the
        // compiler will be smart enough to realize `offset` will never be greater than 1 and use
        // just one register accordingly.
        *(RZ as *mut u8) += offset;
    }
}