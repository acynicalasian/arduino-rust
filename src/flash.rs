/*
    My own library to deal with reading and writing from flash on-system. I may extend this library
    to include IO involving the USART port or IO operations from the SPI (serial programming
    interface).

    There honestly *might* be a HAL provided by arduino_hal or one of its related crates for flash
    storage management but it's so heavily abstracted that I'm having trouble even understanding
    what's going on in those libraries, especially given that I'm just hacking together Rust code
    and m328p assembly.
*/

use panic_halt as _;
use core::arch::asm;

const MAX_MEM_ADDR: u16 = 0x3FFF;
const RZ: u8 = 0x1E;
const R16: u8 = 0x10;

/*
    The static keyword here *should* reserve a byte of RAM that will remain valid throughout the
    lifetime of my program. That means that I won't ever have to worry about which registers the
    compiler decides to use... worrying that I unintentionally overwrote the register being used
    to hold a value read from flash before returning the value read won't be a problem anymore.
 */
static mut READBYTE: u8 = 0;

/*
    Read and return the high byte of the word in flash memory at address `adr`.
 */
#[inline(always)]
pub unsafe fn read_highbyte(adr: u16) -> u8 {
    return read_byte(adr, 1);
}

/*
    Read and return the low byte of the word in flash memory at address `adr`.
 */
#[inline(always)]
pub unsafe fn read_lowbyte(adr: u16) -> u8 {
    return read_byte(adr, 0);
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

#[inline(always)]
pub unsafe fn write_lowbyte(adr: u16, input: u8) {
    return write_byte(adr, 0, input);
}

#[inline(always)]
unsafe fn read_byte(adr: u16, offset: u8) -> u8 {
    if adr > MAX_MEM_ADDR {
        // Halt on failure.
        panic!("Attempted to access invalid flash memory address!");
    }
    unsafe {
        // Set up registers for an LPM instruction.
        asm!(
            "cli",              // Disable interrupts
            "push r30",         // Back up Z-register
            "push r31",
            "push r16",         // Back up r16, we'll use it to hold the byte we read
        );
        // Set the Z-register to our target address. `offset` determines whether we took the high
        // or low byte of the word at `adr`.
        // This code feels insanely hacky since we're operating on two different registers like
        // they're memory, but I don't see how this wouldn't work. It's a lot more readable than
        // taking the "sure" approach of calculating the offset address and assigning the high
        // byte to r31 and the low byte to r30. Hopefully the compiler is smart enough to tell that
        // I want to put the low byte of `offset_adr` into r30 and the high byte into r31.
        let offset_adr = (adr*2);
        *(RZ as *mut u16) = offset_adr;
        // This hacky cast helps ensure that I can pass `offset` as a u8, which makes sure I only
        // use one register. Extremely nitpicky but it could matter? Otherwise, I have to do
        // `*(RZ as *mut u16) = offset_adr + (offset as u16)` because the linter complains about
        // type incompatibility between u16 and u8.
        *(RZ as *mut u8) += offset;
        // Load our target byte into r16 and then into READBYTE.
        asm!(
            "lpm r16, Z",
        );
        READBYTE = *(R16 as *const u8);
        // Fix our registers and reenable interrupts... we only wanted to read one byte.
        asm!(
            "pop r16",
            "pop r31",
            "pop r30",
            "sei",
        );
    }
    return READBYTE;
}

unsafe fn write_byte(adr: u16, offset: u8, input: u8) {
    if adr > MAX_MEM_ADDR {
        // Halt on failure.
        panic!("Attempted to access invalid flash memory address!");
    }
    unsafe {
        
    }
}