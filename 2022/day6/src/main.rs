use std::io::Read;
use std::fs::File;

const SIGNAL_WIDTH: usize = 4;

fn main() {
    let input_data = get_input_data();

    let mut signal_beg = 0;
    let mut signal_end = SIGNAL_WIDTH;
    let mut signal: String = String::from("");

    let found_signal = loop {
        let bytes = input_data.get(signal_beg..signal_end);

        if bytes.is_none() {
            signal_end = 0;
            break false;
        } else {
            let mut candidate = Vec::from(bytes.unwrap());

            candidate.sort_unstable();
            candidate.dedup();

            if candidate.len() == SIGNAL_WIDTH {
                signal = String::from_utf8(bytes.unwrap().to_vec()).expect("Expected UTF-8 bytes");
                break true;
            }
        }

        signal_beg += 1;
        signal_end += 1;
    };

    if found_signal {
        println!("Signal: {signal}. Position: {signal_end}.");
    }
}

fn get_input_data() -> Vec<u8> {
    let mut file_buffer = File::open(".\\data\\input.txt").expect("Could not open input data");
    let mut input_data = String::new();
    file_buffer.read_to_string(&mut input_data).expect("Could not read input data");

    Vec::from(input_data)
}
