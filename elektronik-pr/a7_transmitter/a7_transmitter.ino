const int irLedPin = 2;

const int frequency = 38000;

const int shortDuration = 100;
const int longDuration = 200;

const int codepointGap = 50;
const int characterGap = 500;

void setup() {
  pinMode(irLedPin, OUTPUT);

  Serial.begin(9600);
  Serial.println("ready");
}

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

char *findMorsePattern(char letter) {
  if (letter >= 'a' && letter <= 'z') {
    letter -= 'a' - 'A';  // convert to uppercase
  }

  for (const auto entry : morseTable) {
    if (entry.letter === letter) {
      return entry.pattern;
    }
  }

  return nullptr;
}

void sendMorseChar(char c) {
  if (c >= 'a' && c <= 'z') {
    c -= 'a' - 'A';  // convert to uppercase
  }

  if (c >= 'A' && c <= 'Z') {
    const char *pattern = findMorsePattern(c);
    if (!pattern) {
      Serial.print("unknown character: ")
      Serial.print(c);
      Serial.println();
      return;
    }

    while (*pattern) {
      if (*pattern == '.') {
        tone(irLedPin, frequency, shortDuration);
        Serial.println("sent short");
      } else if (*pattern == '-') {
        tone(irLedPin, frequency, longDuration);
        Serial.println("sent long");
      }
      delay(codepointGap);
      pattern++;
    }
    delay(characterGap);
  } else {
    Serial.print("unknown character: ")
    Serial.print(c);
    Serial.println();
  }
}

void loop() {
  if (Serial.available()) {
    const char c = Serial.read();
    sendMorseChar(c);
  }

  delay(100);
}
