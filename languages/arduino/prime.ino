void setup() {
  Serial.begin(9600); // Start the serial communication
  while (!Serial) {
    ; // Wait for serial port to connect
  }
  
  Serial.println("Enter a number: ");
  
  while (Serial.available() == 0) {
    // Wait for input
  }
  
  int number = Serial.parseInt();
  bool isPrime = true;

  if (number < 2) {
    isPrime = false;
  } else {
    for (int i = 2; i <= number / 2; i++) {
      if (number % i == 0) {
        isPrime = false;
        break;
      }
    }
  }

  if (isPrime) {
    Serial.println("The number is prime.");
  } else {
    Serial.println("The number is not prime.");
  }
}

void loop() {
  // Nothing to do here
}
