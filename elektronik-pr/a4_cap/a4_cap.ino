const int chargePin = 9;
const int dischargePin = 8;

const int voltagePin = A0;

const auto resistorValue = 10000;

void setup() {
  Serial.begin(9600);
  Serial.println("starting");

  pinMode(chargePin, OUTPUT);
  digitalWrite(chargePin, LOW);

  pinMode(dischargePin, OUTPUT);
  digitalWrite(dischargePin, HIGH);
  while (readVoltage() > 0.0) {}
  pinMode(dischargePin, INPUT); // set pin to high impedance

  digitalWrite(chargePin, HIGH);
  const auto startTime = micros();
  while (readVoltage() < 3.15) {} // 5 * 0.63

  const auto timeTaken = micros() - startTime;
  const auto microFarads = timeTaken / resistorValue;

  Serial.println(timeTaken);
  Serial.println(microFarads);
}

void loop() {

}

float readVoltage() {
  return analogRead(voltagePin) * (5.0 / 1023.0);
}
