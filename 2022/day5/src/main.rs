use std::io;
use std::io::prelude::*;
use std::fs::File;

fn main() -> io::Result<()> {
    let mut f = File::open("C:\\Users\\jeffhand\\git\\jeffhandley\\advent-of-code\\2022\\day5\\data\\input.txt")?;
    let mut data = String::new();

    f.read_to_string(&mut data)?;
    let cargo: (Vec<Vec<char>>, Vec<&str>) = parse_data(&data)?;

    let rearranged = process_instructions(cargo.0, cargo.1);

    // let mut top_crates = String::new();
    // for stack in rearranged {
    //     let top = stack.to_vec().pop().expect("Stack is empty");
    //     top_crates.push(top);
    // }

    // println!("Top Crates: {top_crates}");

    print_stacks(rearranged);

    Ok(())
}

fn parse_data(data: &str) -> io::Result<(Vec<Vec<char>>, Vec<&str>)> {
    let lines: Vec<&str> = data.lines().collect();
    println!("Lines of input data: {}", lines.len());

    let mut crates: Vec<&str> = Vec::new();

    for line in &lines {
        // There is a blank line between the crate stacks
        // and the series of instructions.
        if line.trim().len() == 0 {
            // The bottom line in the crate stacks is the
            // index of stacks; we can ignore that line.
            crates.pop();
            break;
        }

        crates.push(line);
    }


    let tallest_stack = crates.len();
    println!("Tallest stack of crates: {}", tallest_stack);
    let crate_stacks = parse_crates(crates);

    let mut instructions: Vec<&str> = Vec::new();

    for line in &lines[(tallest_stack+2)..] {
        let line = line.trim();

        if line.len() == 0 {
            break;
        }

        instructions.push(line);
    }

    let num_instructions = instructions.len();
    println!("Number of instructions: {}", num_instructions);

    Ok((crate_stacks, instructions))
}

fn parse_crates(mut crates: Vec<&str>) -> Vec<Vec<char>> {
    // Turn the crate stacks upside down to get the bottom row first
    crates.reverse();

    let mut stacks: Vec<Vec<char>> = Vec::new();

    for row in crates {
        let mut columns: Vec<char> = row.chars().collect();
        columns.reverse();

        let mut col: usize = 0;
        while columns.len() >= 3 {
            columns.pop();
            let item = columns.pop().unwrap();
            columns.pop();

            if columns.len() > 0 {
                columns.pop();
            }

            let mut stack: Vec<char> = stacks.get_mut(col).unwrap_or(&mut Vec::<char>::new()).to_vec();

            if String::from(item).trim().len() > 0 {
                stack.push(item);
                println!("Added {} to stack {}. New size: {}.", item, col, stack.len());
            }

            if stacks.len() == col {
                stacks.push(stack)
            }

            col += 1;
        }
    }

    stacks
}

fn process_instructions(cargo: Vec<Vec<char>>, instructions: Vec<&str>) -> Vec<Vec<char>> {
    let mut stacks: Vec<Vec<char>> = Vec::new();

    for stack in cargo {
        let mut crates: Vec<char> = Vec::new();

        for item in stack {
            crates.push(item);
        }

        stacks.push(crates);
    }

    for inst in instructions {
        let mut parts = inst.split_ascii_whitespace();
        parts.next(); // move
        let mut count: u8 = parts.next().unwrap_or("0").parse().unwrap_or(0);
        parts.next(); // from
        let old: usize = parts.next().unwrap_or("0").parse().unwrap_or(0);
        parts.next(); // to
        let new: usize = parts.next().unwrap_or("0").parse().unwrap_or(0);

        if count > 0 && old > 0 && new > 0 {
            println!("Moving {count} from {old} to {new}");

            while count > 0 {
                // Stacks are 1-based in the data
                // let mut old_stack = stacks.get(old - 1).expect("Could not get old stack").to_vec();
                // let mut new_stack = stacks.get(new - 1).expect("Could not get new stack").to_vec();

                // let item = old_stack.pop().expect("No crates left in stack");
                // new_stack.push(item);

                count -= 1;
            }
        }
    }

    stacks
}

fn print_stacks(cargo: Vec<Vec<char>>) {
    for stack in cargo {
        for item in stack {
            print!("[{item}] ");
        }
        println!();
    }
}
