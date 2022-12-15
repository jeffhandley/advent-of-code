use std::io::Read;
use std::fs::File;

fn main() {
    let input_data = get_input_data();
    let mut size: usize = 2;
    let beg: usize = 0;

    loop {
        let (found, beg, end, seq) = find_first_unique_bytes(&input_data, size, beg);

        if !found {
            break;
        }

        let range = format!("{beg}..{end}");
        println!("Size {size:>3}: {range:>12} - {seq}");

        size += 1;
    }
}

fn find_first_unique_bytes(data: &Vec<u8>, size: usize, mut beg: usize) -> (bool, usize, usize, String) {
    let mut end = size;
    let mut seq: String = String::from("");

    let found_signal = loop {
        let bytes = data.get(beg..end);

        if bytes.is_none() {
            break false;
        } else {
            let mut candidate = Vec::from(bytes.unwrap());

            candidate.sort_unstable();
            candidate.dedup();

            if candidate.len() == size {
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
