const int irSensorPin = 2;

const int shortDurationMin = 90;
const int shortDurationMax = 110;

const int longDurationMin = 180;
const int longDurationMax = 220;

const int characterGapMin = 450;

const int wordGapMin = 650;

void setup() {
  pinMode(irSensorPin, INPUT);

  Serial.begin(9600);
  Serial.println("ready");
}

String morseBuffer = "";
unsigned long lastSignalTime = 0;
bool receiving = false;

struct MorseCode {
  const char* pattern;
  char letter;
};

MorseCode morseTable[] = {
  { ".-", 'A' }, { "-...", 'B' }, { "-.-.", 'C' }, { "-..", 'D' }, 
  { ".", 'E' }, { "..-.", 'F' }, { "--.", 'G' }, { "....", 'H' }, 
  { "..", 'I' }, { ".---", 'J' }, { "-.-", 'K' }, { ".-..", 'L' }, 
  { "--", 'M' }, { "-.", 'N' }, { "---", 'O' }, { ".--.", 'P' }, 
  { "--.-", 'Q' }, { ".-.", 'R' }, { "...", 'S' }, { "-", 'T' }, 
  { "..-", 'U' }, { "...-", 'V' }, { ".--", 'W' }, { "-..-", 'X' }, 
  { "-.--", 'Y' }, { "--..", 'Z' },
};

char decodeMorse(const String& pattern) {
  for (const auto entry : morseTable) {
    if (pattern == entry.pattern) {
      return entry.letter;
    }
  }
  return '?';
}

void loop() {
  const auto signal = digitalRead(irSensorPin);
  const auto now = millis();

  if (signal == LOW) {
    if (!receiving) {
      lastSignalTime = now;
      receiving = true;
    }
  } else if (receiving) {
    // signal is not low but we still have state == receiving
    //  -> transmission ended just now but we haven't handled it yet

    const auto duration = now - lastSignalTime;
    receiving = false;

    if (duration > shortDurationMin && duration < shortDurationMax) {
      morseBuffer += ".";
    } else if (duration > longDurationMin && duration < longDurationMax) {
      morseBuffer += "-";
    } else if (duration >= characterGapMin) {
      const auto decodedChar = decodeMorse(morseBuffer);
      Serial.print(decodedChar);
      morseBuffer = "";
    }
  }
}
