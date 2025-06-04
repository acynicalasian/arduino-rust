/*
    This is going to be a hacky bootloader. The plan is to make main.rs a file that doesn't change
    across "firmware versions". Here, we'll define debug strings, code the logic to recover from 
    bad firmware updates, etc. Finally, we'll call a main_routine() imported from main_routine that
    contains the behavior that'll change across FW versions.
*/

// Standard attributes, we need the last two features as we're going to need inline assembly and
// likely direct memory access to specific addresses in SRAM.
#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(raw_ref_op)]

use panic_halt as _;

use avr_device::interrupt;              // Convenient to set up critical sections. Can probably be done
                                        // by disabling interrupts globally with an assembly command,
                                        // but this could lead to more readable code.
use core::cell::RefCell;                // This use directive is probably needed for our console output
                                        // macro.
use core::arch::asm;                    // For inline assembly.

// Some syntactic sugar for using our main_routine() from src/main_routine.rs.
mod main_routine;
use main_routine::main_routine as main_routine;

mod flash;
use flash::read_highbyte as read_highbyte;
use flash::read_lowbyte as read_lowbyte;


/*
    Define variables and functions needed for serial output.
 */
type Console = arduino_hal::hal::usart::Usart0<arduino_hal::DefaultClock>;
static CONSOLE: interrupt::Mutex<RefCell<Option<Console>>> =
    interrupt::Mutex::new(RefCell::new(None));

macro_rules! println {
    ($($t:tt)*) => {
        interrupt::free(
            |cs| {
                if let Some(console) = CONSOLE.borrow(cs).borrow_mut().as_mut() {
                    let _ = ufmt::uwriteln!(console, $($t)*);
                }
            },
        )
    };
}

fn put_console(console: Console) {
    interrupt::free(|cs| {
        *CONSOLE.borrow(cs).borrow_mut() = Some(console);
    });
}

#[arduino_hal::entry]
fn main() -> ! {
    // Set up our HAL and serial output. I don't know a better way to debug our Arduino other than
    // using serial output.
    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);
    let serial = arduino_hal::default_serial!(dp, pins, 57600);
    put_console(serial);

    // Testing: mark this area with a couple sets to good ol 69
    interrupt::free(|_| {
        unsafe {
            asm!(
                "push r16",
                "push r17",
                "push r18",
                "ldi r16, 69",
                "ldi r17, 69",
                "ldi r18, 69",
                "pop r18",
                "pop r17",
                "pop r16",
            );
        }
        let whathappens = "well what happens here?";
        unsafe {
            asm!(
                "push r16",
                "push r17",
                "push r18",
                "ldi r16, 69",
                "ldi r17, 69",
                "ldi r18, 69",
                "pop r18",
                "pop r17",
                "pop r16",
            );
        }
        println!("{}", whathappens);
    });
    main_routine();
}
