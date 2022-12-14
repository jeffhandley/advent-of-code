use std::collections::HashMap;
use std::io;
use std::io::prelude::*;
use std::fs::File;

fn main() -> io::Result<()> {
    let mut f = File::open(".\\data\\input.txt")?;
    let mut data = String::new();

    f.read_to_string(&mut data)?;
    let cargo: (HashMap<usize, Vec<char>>, Vec<&str>) = parse_data(&data)?;
    let original = cargo.0;
    let instructions = cargo.1;

    print_stacks(&original);
    let rearranged = process_instructions(&original, &instructions, false);
    print_stacks(&rearranged);
    println!("Top Crates (Part One): {}", top_crates(&rearranged));

    print_stacks(&original);
    let rearranged = process_instructions(&original, &instructions, true);
    print_stacks(&rearranged);
    println!("Top Crates (Part Two): {}", top_crates(&rearranged));

    Ok(())
}

fn top_crates(crates: &HashMap<usize, Vec<char>>) -> String {
    let mut top_crates = String::new();

    let mut cols: Vec<usize> = crates.keys().copied().collect();
    cols.sort_unstable();

    for col in cols {
        let stack: Vec<char> = crates.get(&col).expect("Should have this key").to_vec();
        let top_crate: &char = stack.last().unwrap_or(&' ');

        top_crates.push(*top_crate);
    }

    top_crates
}

fn parse_data(data: &str) -> io::Result<(HashMap<usize, Vec<char>>, Vec<&str>)> {
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

fn parse_crates(mut crates: Vec<&str>) -> HashMap<usize, Vec<char>> {
    // Turn the crate stacks upside down to get the bottom row first
    crates.reverse();

    let mut stacks: HashMap<usize, Vec<char>> = HashMap::new();

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

            let mut stack: Vec<char> = stacks.get(&col).unwrap_or(&Vec::<char>::new()).to_vec();

            if String::from(item).trim().len() > 0 {
                stack.push(item);
                // println!("Added {} to stack {}. New size: {}.", item, col, stack.len());

                stacks.insert(col, stack);
                print_stacks(&stacks);
            }

            col += 1;
        }
    }

    stacks
}

fn process_instructions(cargo: &HashMap<usize, Vec<char>>, instructions: &Vec<&str>, move_multi: bool) -> HashMap<usize, Vec<char>> {
    let mut stacks: HashMap<usize, Vec<char>> = HashMap::new();

    for (col, stack) in cargo {
        let mut crates: Vec<char> = Vec::new();

        for item in stack {
            crates.push(*item);
        }

        stacks.insert(*col, crates);
    }

    // let stdin = io::stdin();

    for inst in instructions {
        let mut parts = inst.split_ascii_whitespace();
        parts.next(); // move
        let count: u8 = parts.next().unwrap_or("0").parse().unwrap_or(0);
        parts.next(); // from
        let old: usize = parts.next().unwrap_or("0").parse().unwrap_or(0);
        parts.next(); // to
        let new: usize = parts.next().unwrap_or("0").parse().unwrap_or(0);

        if count > 0 && old > 0 && new > 0 {
            // println!("Moving {count} from {old} to {new}");

            // Stacks are 1-based in the data
            let mut old_stack = stacks.get(&(old - 1)).expect("Could not get old stack").to_vec();
            let mut new_stack = stacks.get(&(new - 1)).expect("Could not get new stack").to_vec();

            let start = old_stack.len() - usize::from(count);
            let mut items = old_stack.drain(start..).as_slice().to_vec();

            if !move_multi {
                items.reverse();
            }

            new_stack.append(&mut items);

            stacks.insert(old - 1, old_stack);
            stacks.insert(new - 1, new_stack);

            // print_stacks(&stacks);

            // let mut input = String::new();
            // stdin.read_line(&mut input).ok();
        }
    }

    stacks
}

fn print_stacks(cargo: &HashMap<usize, Vec<char>>) {
    let mut cols: Vec<usize> = cargo.keys().copied().collect();
    cols.sort_unstable();

    let mut height: usize = 0;
    let width: usize = cargo.keys().len();

    for (_, stack) in cargo {
        let this_height = stack.len();

        if height < this_height {
            height = this_height;
        }
    }

    for row in (0..height).rev() {
        for col in 0..width {
            let item = cargo.get(&col).expect("Column expected").get(row);

            if item.is_some() {
                print!("[{}] ", item.unwrap());
            } else {
                print!("    ");
            }
        }

        println!();
    }

    for col in 0..width {
        print!(" {}  ", col + 1);
    }

    println!();
}
