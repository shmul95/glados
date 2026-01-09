package main

import "fmt"

func main() {
	totalSteps := int64(0)

	for i := int64(1); i <= 1_000_000; i++ {
		n := i

		for n != 1 {

			if n%2 == 0 {
				n = n / 2
			} else {
				n = 3*n + 1
			}

			totalSteps++
		}
	}

	fmt.Print(totalSteps)
}
