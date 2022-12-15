use std::io::Read;
use std::fs::File;

fn main() {
    let input_data = get_input_data();
    let (signal_found, signal_beg, signal_end, signal) = find_first_unique_bytes(&input_data, 4);
    let (message_found, message_beg, message_end, message) = find_first_unique_bytes(&input_data, 14);

    if signal_found {
        println!("Signal: {signal}. Position: {signal_beg}-{signal_end}.");
    }

    if message_found {
        println!("Message: {message}. Position: {message_beg}-{message_end}.");
    }
}

fn find_first_unique_bytes(data: &Vec<u8>, signal_width: usize) -> (bool, usize, usize, String) {
    let mut beg = 0;
    let mut end = signal_width;
    let mut seq: String = String::from("");

    let found_signal = loop {
        let bytes = data.get(beg..end);

        if bytes.is_none() {
            break false;
        } else {
            let mut candidate = Vec::from(bytes.unwrap());

            candidate.sort_unstable();
            candidate.dedup();

            if candidate.len() == signal_width {
                seq = String::from_utf8(bytes.unwrap().to_vec()).expect("Expected UTF-8 bytes");
                break true;
            }
        }

        beg += 1;
        end += 1;
    };

    if found_signal {
        return (true, beg, end, seq);
    }

    return (false, 0, 0, seq);
}

fn get_input_data() -> Vec<u8> {
    let mut file_buffer = File::open(".\\data\\input.txt").expect("Could not open input data");
    let mut input_data = String::new();
    file_buffer.read_to_string(&mut input_data).expect("Could not read input data");

    Vec::from(input_data)
}
