#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(raw_ref_op)]

/*
    I worked on this file to check the fuse and lock bits of the bootloader.
 */

use panic_halt as _;

// Everything between here and the entrypoint are copied from the arduino-hal repo
use avr_device::interrupt;
use core::cell::RefCell;
use core::arch::asm;

type Console = arduino_hal::hal::usart::Usart0<arduino_hal::DefaultClock>;
static CONSOLE: interrupt::Mutex<RefCell<Option<Console>>> =
    interrupt::Mutex::new(RefCell::new(None));

macro_rules! print {
    ($($t:tt)*) => {
        interrupt::free(
            |cs| {
                if let Some(console) = CONSOLE.borrow(cs).borrow_mut().as_mut() {
                    let _ = ufmt::uwrite!(console, $($t)*);
                }
            },
        )
    };
}

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
    })
}

#[arduino_hal::entry]
fn main() -> ! {
    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);
    let serial = arduino_hal::default_serial!(dp, pins, 57600);
    put_console(serial);


    // This should load the lock bits into r0.
    unsafe {
        asm!(
            "ldi r30, 1",
            "ldi r31, 0",
            "ldi r16, 7",
            "sts 0x57, r16",
            "lpm",
        );
    }

    let r0addr = 0usize;
    let r0 = &raw const r0addr;
    
    let mut led = pins.d13.into_output();

    loop {
        led.toggle();
        arduino_hal::delay_ms(1000);
        unsafe {
            println!("r0 is {}", *r0);
        }
    }
}
