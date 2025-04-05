#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]
#![feature(raw_ref_op)]

/*
    Set the lock bit BLB12 to 1 to enable reads of the bootloader section of flash memory.
    Dump the contents and transfer it via usart to examine it on our dev computer.
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
    // We're going to be referring to the ATmega328p docs to set BLB12 to 1, so let's run our code ahead
    // of the setup code to reduce the chances of us mucking up registers.
    interrupt::free(|_| {
        unsafe {
            asm!(
                // Back up r0's value to reduce the chance we mucked up registers.
                "push r0",
                // Back up r16 since we're going to use it to back up SPMCSR's value.
                "push r16",
                // Back up r17 since apparently the ORI instruction doesn't work on low regs. Use r17 to
                // write to r0 later.
                "push r17",
                // Just in case, back up the Z-ptr since SPM instructions typically write to it.
                "push r30",
                "push r31",
                // Back up SPMCSR to r16.
                "in r16, 0x37",
                // Set up r0 to hold the proper lock bit values. 0b11101111
                "ldi r17, 0xEF",
                "mov r0, r17",
                // "For future compatibility, it is recommended to load the Z-pointer with 0x0001"
                "ldi r30, 1",
                "ldi r31, 0",
                // Write "X0001001" to SPMCSR. I'm assuming this means we just keep the first bit of
                // SPMCSR no matter what. Keep the first bit of SPMCSR in r16 with an ANDI instruction.
                "andi r16, 0x80",
                "ori r16, 0x9",
                "out 0x37, r16",
                "spm",
                // We should have set BLB12 to 1 now. Reset our registers to the previous state to be safe.
                "pop r31",
                "pop r30",
                "pop r17",
                "pop r16",
                "pop r0",
            );
        }
    });

    // Now we'll set up our HAL.
    let dp = arduino_hal::Peripherals::take().unwrap();
    let pins = arduino_hal::pins!(dp);
    let serial = arduino_hal::default_serial!(dp, pins, 57600);
    put_console(serial);

    let mut led = pins.d13.into_output();

    println!("we done");

    loop {
        led.toggle();
        arduino_hal::delay_ms(1000);
    }
}
