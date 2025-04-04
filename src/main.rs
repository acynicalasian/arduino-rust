#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(raw_ref_op)]

/*
    I worked on this file to check the fuse and lock bits of the bootloader. I need to know whether
    the bootloader section of the flash memory is protected or not... I believe the boot lock bits
    can show me that. I also need the bits noting the size of the bootloader for future reference;
    I can't trust outside sources for this because I have an older hardware revision that may not
    use the Optiboot bootloader which, to my knowledge, is used by default from 2019 onwards. Also,
    I could run into weird trouble because I'm running Rust on Arduino instead of using Arduino C++
    with the Arduino IDE.
 */

use panic_halt as _;

use avr_device::interrupt;
use core::cell::RefCell;
use core::arch::asm;

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
    // We need to run this code to extract boot and fuse bits before any of the setup code occurs.
    // Running raw assembly after this point completely FUBARs how code after it runs in unpredictable
    // ways, but since we're running bare metal code at this point, we should be fine if we run what
    // assembly we need prior to the setup code runnning, because we don't need higher level abstracted
    // access to the board's other functions. The compiler should take care of storing variables in a
    // way that doesn't FUBAR the code after this, so we should be fine.

    // Read the lock bits: refer to page 235 of the ATmega328p documentation:
    // https://ww1.microchip.com/downloads/aemDocuments/documents/MCU08/ProductDocuments/DataSheets/Atmel-7810-Automotive-Microcontrollers-ATmega328P_Datasheet.pdf

    let mut lockbits: u8 = 0;
    let ptr_lockbits = &raw mut lockbits;

    // We need to run our assembly code in critical sections to avoid side effects.
    interrupt::free(|_| {
        unsafe {
            asm!(
                // Load Z-pointer with 0x0001
                "ldi r30, 1",
                "ldi r31, 0",
                // Set BLBSET and SELFPRGEN bits in SPMCSR IO register (bits 0 and 3)
                // To avoid side effects, copy the SPMCSR value to r16; logical OR it with 0b1001; copy
                // r16 into SPMCSR to avoid touching the other bits. I don't see a way to do it a faster
                // way in the assembly lang documentation.
                "in r16, 0x37",
                "ori r16, 9",
                "out 0x37, r16",
                // Run an LPM instruction to load the boot bits into our target register (we'll use r17)
                "lpm r17, Z",
            );
            // Save our value from r17.
            *ptr_lockbits = *(0x11 as *const u8);
        }
    });
    
    // Read the fuse high byte to see bootloader size: refer to page 236 of the Atmega328p docs.
    let mut bootbits: u8 = 0;
    let ptr_bootbits = &raw mut bootbits;
    interrupt::free(|_| {
        unsafe {
            asm!(
                // Load Z-ptr with 0x0003
                "ldi r30, 3",
                "ldi r31, 0",
                // Set BLBSET and SELFPRGEN bits in SPMCSR IO register again; they autoclear when we
                // read the values using an LPM instruction.
                "in r18, 0x37",
                "ori r18, 9",
                "out 0x37, r18",
                "lpm r19, Z",
            );
            *ptr_bootbits = *(0x13 as *const u8);
        }
    });

    // Drop the two high bits from lockbits. They could be initiated as 1 by default.
    lockbits = lockbits & 0b00111111;

    // Now we'll set up our HAL.

    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);

    // This should set up our program to transmit across the USB port.
    let serial = arduino_hal::default_serial!(dp, pins, 57600);
    put_console(serial);

    println!("Value of lock bits: {}", lockbits);
    println!("Value of boot bits: {}", bootbits);

    let mut led = pins.d13.into_output();

    loop {
        led.toggle();
        arduino_hal::delay_ms(1000);
    }
}
