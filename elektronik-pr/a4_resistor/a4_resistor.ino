const int analogPin = A0;  
const float R = 10000.0;
const float Ue = 5.0;

void setup() {
    Serial.begin(9600);

    int value = analogRead(analogPin);
    float UA = value * (Ue / 1023.0);
    float Rx = R * (UA / (Ue - UA));
    
    Serial.print(Rx);
    Serial.println(" O");
}

void loop() {
  // no-op
}
