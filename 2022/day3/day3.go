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

    fmt.Println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

    numSacks := len(rucksacks)
    totalBadgePriority := 0

    for i := 0; i + 2 < numSacks; i += 3 {
        badge := findBadge(rucksacks[i], rucksacks[i + 1], rucksacks[i + 2])
        char := string(badge)
        code := int(badge)
        badgePriority := getPriority(badge)
        totalBadgePriority += badgePriority

        fmt.Printf("Group %d: %s (%d) = (%d)\n", (i / 3) + 1, char, code, badgePriority)
    }

    fmt.Println()
    fmt.Println("Total Priority (Part One):", totalPriority)
    fmt.Println("Total Badge Priority (Part Two):", totalBadgePriority)
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

func findBadge(elf1, elf2, elf3 string) rune {
    for _, itemtype := range elf1 {
        if strings.IndexRune(elf2, itemtype) >= 0 && strings.IndexRune(elf3, itemtype) >= 0 {
            return itemtype
        }
    }

    return rune(0)
}
