const int leftBtnPin = 2;
const int rightBtnPin = 3;

const int statusRedLedPin = 4;
const int statusGreenLedPin = 5;

const int ultraTriggerPin = 6;
const int ultraPulsePin = 13;

const int serPin = 8;
const int srclkPin = 9;
const int rclkPin = 10;

const int motorDriver1APin = 11;
const int motorDriver2APin = 12;

const int maxSpeed = 10;

int speed = 0;
int direction = 1;

bool motorEnabled = true;

void setup() {
  Serial.begin(9600);

  pinMode(serPin, OUTPUT);
  pinMode(srclkPin, OUTPUT);
  pinMode(rclkPin, OUTPUT);

  pinMode(leftBtnPin, INPUT);
  pinMode(rightBtnPin, INPUT);

  pinMode(statusRedLedPin, OUTPUT);
  pinMode(statusGreenLedPin, OUTPUT);

  pinMode(ultraTriggerPin, OUTPUT);
  pinMode(ultraPulsePin, INPUT);

  pinMode(motorDriver1APin, OUTPUT);
  pinMode(motorDriver2APin, OUTPUT);

  digitalWrite(srclkPin, HIGH);

  attachInterrupt(digitalPinToInterrupt(leftBtnPin), onLeftButton, RISING);
  attachInterrupt(digitalPinToInterrupt(rightBtnPin), onRightButton, RISING);
}

const int minDistance = 30;

void loop() {
  updateMotorStatusLED();

  const float distance = getDistanceCM();
  Serial.print("distance ");
  Serial.println(distance);

  // distance 0 is invalid measurement
  motorEnabled = distance == 0 || distance > minDistance;
  // motorEnabled = true;

  updateMotor();
}

float getDistanceCM() {
    digitalWrite(ultraTriggerPin, HIGH);
    delayMicroseconds(2);
    digitalWrite(ultraTriggerPin, LOW);
    delayMicroseconds(20); // spec says 10 but let's be safe
    digitalWrite(ultraTriggerPin, HIGH);
    
    long duration = pulseIn(ultraPulsePin, HIGH);
    float distance = duration * 0.034 / 2;
    return distance;
}

void updateMotorStatusLED() {
  bool running = speed != 0 && motorEnabled;
  digitalWrite(statusRedLedPin, running ? HIGH : LOW);
  digitalWrite(statusGreenLedPin, running ? LOW : HIGH);
  printNumber(speed);
}

void printNumber(int number) {
  switch (number) {
    case 0:
      sendByte(0xF7);
      break;
    case 1:
      sendByte(0b00100100);
      break;
    case 2:
      sendByte(0b00111011);
      break;
    case 3:
      sendByte(0b00111101);
      break;
    case 4:
      sendByte(0b01101100);
      break;
    case 5:
      sendByte(0b01011101);
      break;
    case 6:
      sendByte(0b00111111);
      break;
    case 7:
      sendByte(0b00110100);
      break;
    case 8:
      sendByte(0b01111111);
      break;
    case 9:
      sendByte(0b01111101);
      break;
  }
}

void sendByte(byte b) {
  digitalWrite(rclkPin, LOW);
  shiftOut(serPin, srclkPin, MSBFIRST, b);
  digitalWrite(rclkPin, HIGH);
}

void onLeftButton() {
  Serial.println("left button");
  Serial.println(speed);

  direction = -1;
  speed = (speed + 1) % (maxSpeed);

}

void onRightButton() {
  Serial.println("right button");
  Serial.println(speed);

  direction = 1;
  speed = (speed + 1) % (maxSpeed);
}

void updateMotor() {
  if (motorEnabled && speed > 0) {
    int highPin = direction == 1 ? motorDriver1APin : motorDriver2APin;
    int lowPin = direction == 1 ? motorDriver2APin : motorDriver1APin;

    Serial.println((255.0 / maxSpeed) * speed);
    Serial.println(direction == 1 ? "dir1" : "dir2");

    analogWrite(highPin, ((205.0 / maxSpeed) * speed) + 50);
    analogWrite (lowPin, 0);
  } else {
    analogWrite(motorDriver1APin, 255);
    analogWrite(motorDriver2APin, 255);
  }
}
