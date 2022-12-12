package main

import (
    "fmt"
    "strings"
    "day3/inputdata"
)

func main() {
    rucksacks := inputdata.GetRucksacks()
    arrangements := getArrangements(rucksacks)
    totalPriority := 0

    for _, rucksack := range arrangements {
        fmt.Printf("%s | %s\n", rucksack.Compartment1, rucksack.Compartment2)

        for _, shared := range rucksack.Shared {
            char := string(shared)
            code := int(shared)
            priority := getPriority(shared)

            totalPriority += priority

            fmt.Printf("    - %s (%d) = %d\n", char, code, priority)
        }
    }

    fmt.Println()
    fmt.Println("Total Priority:", totalPriority)
}

type Arrangement struct {
    Compartment1 string
    Compartment2 string
    Shared []rune
}

func getPriority(itemtype rune) int {
    const uppercaseShift = 65 - 27
    const lowercaseShift = 97 - 1

    code := int(itemtype)

    switch {
        case code >= 65 && code <= 90:  return code - uppercaseShift
        case code >= 97 && code <= 122: return code - lowercaseShift
    }

    return 0
}

func getArrangements(rucksacks []string) []Arrangement {
    arrangements := []Arrangement {}

    for _, sack := range rucksacks {
        count := len(sack)
        compartment1 := sack[:(count/2)]
        compartment2 := sack[(count/2):]

        sharedMap := map[rune]bool {}
        shared := []rune {}

        for _, itemtype := range compartment1 {
            if strings.ContainsRune(compartment2, itemtype) && !sharedMap[itemtype] {
                sharedMap[itemtype] = true
                shared = append(shared, itemtype)
            }
        }

        arrangements = append(arrangements, Arrangement { compartment1, compartment2, shared })
    }

    return arrangements
}
