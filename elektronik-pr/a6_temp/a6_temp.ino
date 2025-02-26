const int displayDataPin = 8;
const int displayClockPin = 9;
const int displayLatchPin = 10;

const int motorSpeedPin = 5;

// todo: these probably need a different pin
const int upButtonPin = 4;
const int downButtonPin = 5;

const int greenLedPin = 6;
const int redLedPin = 8;

volatile int currentSpeed = 0;

void setup() {
  Serial.begin(9600);

  // put your setup code here, to run once:
  pinMode(displayDataPin, OUTPUT);
  pinMode(displayClockPin, OUTPUT);
  pinMode(displayLatchPin, OUTPUT);

  // pinMode(upButtonPin, INPUT);
  // pinMode(downButtonPin, INPUT);

  // pinMode(greenLedPin, OUTPUT);
  // pinMode(redLedPin, OUTPUT);

  // digitalWrite(displayClockPin, LOW);

  // digitalWrite(displayLatchPin, LOW);
  shiftOut(displayDataPin, displayLatchPin, LSBFIRST, 0b00000000);
  // digitalWrite(displayLatchPin, HIGH);

  // attachInterrupt(digitalPinToInterrupt(upButtonPin), upButtonPressed, FALLING);
  // attachInterrupt(digitalPinToInterrupt(downButtonPin), downButtonPressed, FALLING);
}

byte value = 0b10000000;

void loop() {
  // put your main code here, to run repeatedly:

  digitalWrite(displayLatchPin, LOW);
  shiftOut(displayDataPin, displayClockPin, LSBFIRST, value);
  digitalWrite(displayLatchPin, HIGH);

  delay(1000);

  // Serial.println(value);

  // value  = value << 1;
  // if (!value) {
  //   value = 0b11111111;
  // }

  // delay(500);
}

void upButtonPressed() {
  currentSpeed = (currentSpeed + 1) % 10;
}

void downButtonPressed() {
  currentSpeed = (currentSpeed - 1) % 10;
}

void displaySend() {
  digitalWrite(displayLatchPin, LOW);
  digitalWrite(displayLatchPin, HIGH);
}

