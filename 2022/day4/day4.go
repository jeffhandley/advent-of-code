package main

import ("fmt";"day4/inputdata")

func main() {
    pairs := inputdata.GetPairRanges()
    fmt.Println("Pairs:", len(pairs))

    fullyContained := 0

    for _, pair := range pairs {
        if (pair.Elf1.Min <= pair.Elf2.Min && pair.Elf1.Max >= pair.Elf2.Max) ||
           (pair.Elf2.Min <= pair.Elf1.Min && pair.Elf2.Max >= pair.Elf1.Max) {
            fmt.Printf("Overlap: %d-%d and %d-%d\n", pair.Elf1.Min, pair.Elf1.Max, pair.Elf2.Min, pair.Elf2.Max)
            fullyContained += 1
        }
    }

    fmt.Println("Complete Overlaps:", fullyContained)
}
