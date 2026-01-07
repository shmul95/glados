package main

import (
	"fmt"
)

func main() {
	result := 0
	for i := range 1_000_000_000 {
		result += i
	}
	fmt.Println(result)
}
