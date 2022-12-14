use std::io;
use std::io::prelude::*;
use std::fs::File;

fn main() -> io::Result<()> {
    let mut f = File::open("C:\\Users\\jeffhand\\git\\jeffhandley\\advent-of-code\\2022\\day5\\data\\input.txt")?;
    let mut data = String::new();

    f.read_to_string(&mut data)?;
    let cargo: (Vec<Vec<char>>, Vec<&str>) = parse_data(&data)?;

    for inst in &cargo.1 {
        println!("{}", inst);
    }

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

            let mut stack: Vec<char> = stacks.get(col).unwrap_or(&Vec::<char>::new()).to_vec();

            if String::from(item).trim().len() > 0 {
                println!("Adding {} to stack {}", item, col);
                stack.push(item);
            }

            if stacks.len() == col {
                stacks.push(stack)
            }

            col += 1;
        }
    }

    stacks
}
