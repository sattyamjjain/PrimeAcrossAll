package main

import (
	"fmt"
	"strconv"

	"github.com/hyperledger/fabric-contract-api-go/contractapi"
)

// SmartContract provides functions for checking prime numbers
type SmartContract struct {
	contractapi.Contract
}

// IsPrime checks if a number is prime
func (s *SmartContract) IsPrime(ctx contractapi.TransactionContextInterface, number string) (bool, error) {
	num, err := strconv.Atoi(number)
	if err != nil {
		return false, err
	}

	if num <= 1 {
		return false, nil
	}
	for i := 2; i*i <= num; i++ {
		if num%i == 0 {
			return false, nil
		}
	}
	return true, nil
}

func main() {
	chaincode, err := contractapi.NewChaincode(new(SmartContract))
	if err != nil {
		fmt.Printf("Error creating prime checker chaincode: %s", err.Error())
	}

	if err := chaincode.Start(); err != nil {
		fmt.Printf("Error starting prime checker chaincode: %s", err.Error())
	}
}
