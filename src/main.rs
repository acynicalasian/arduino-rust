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

// macro_rules! print {
//     ($($t:tt)*) => {
//         interrupt::free(
//             |cs| {
//                 if let Some(console) = CONSOLE.borrow(cs).borrow_mut().as_mut() {
//                     let _ = ufmt::uwrite!(console, $($t)*);
//                 }
//             },
//         )
//     };
// }

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

    // This should set up our program to transmit across the USB port.
    let serial = arduino_hal::default_serial!(dp, pins, 57600);
    put_console(serial);

    // Testing... Are we reading properly from registers?
    // We might need to prevent interrupts here to make sure our register reads
    // are completely syncrhonous.
    interrupt::free(|_| {
        unsafe {
            asm!(
                "ldi r16, 0x45",
                "sts 0x08f7, r16",
            );
        }
        // I wasn't paying attention and had r16 = 0x16. That's why we were reading 0.
        let r16 = 0x10 as *const u16;
        // We expect the print values to output 69.
        unsafe {
            println!("r16 is: {}", *r16);
            println!("value in SRAM: {}", *(0x08f7 as *const u16) as u8);
        }
    });
    

    // We'll load the fuse high byte into r0 first to get the BOOTSZ fuse bits.
    // SPMCSR IO register cannot be accessed directly by SBIS instructions, and
    // I don't want to mess with anything, so we'll have to copy its values and
    // OR the value.
    //
    // Load SPMCSR's value into r16.
    // Calculate the value to go into SPMCSR by ORing r116 and 0b00001001.
    // Load 0x0003 into the Z register.
    // Load our calculated value into SPMCSR, thereby setting the BLBSET and
    // SELFPRGEN bits.
    // Run an LPM instruction to load the fuse high bits into r0.
    unsafe {
        asm!(
            "in r16, 0x37",
            "ori r16, 7",
            "ldi r30, 3",
            "ldi r31, 0",
            "out 0x37, r16",
            "lpm",
        );
    }

    let r0 = 0 as *const u16;
    
    let mut led = pins.d13.into_output();

    loop {
        led.toggle();
        arduino_hal::delay_ms(1000);
        unsafe {
            println!("r0 is {}", *r0);
        }
    }
}
