uint16_t wave_values[] = {
    0x200, 0x2A3, 0x332, 0x399, 0x3EB, 0x3FF, 0x3EB, 0x399, 
    0x332, 0x2A3, 0x200, 0x15C, 0x0CC, 0x066, 0x014, 0x000, 
    0x014, 0x066, 0x0CC, 0x15C
};
int wave_length = 20;

int frequencies[] = { 262, 294, 330, 349, 392, 440, 494 };

void setup() {
  // put your setup code here, to run once:
  DDRB = 0xFF;
  Serial.begin(9600);
}

void loop() {
  // put your main code here, to run repeatedly:
  String input = Serial.readString();
  for (int i = 0; i < input.length(); i++) {
    switch (input.charAt(i)) {
      case 'c': 
        emit_sound(frequencies[0], 500);
        break;
      case 'd': 
        emit_sound(frequencies[1], 500);
        break;
      case 'e': 
        emit_sound(frequencies[2], 500);
        break;
      case 'f': 
        emit_sound(frequencies[3], 500);
        break;
      case 'g': 
        emit_sound(frequencies[4], 500);
        break;
      case 'a': 
        emit_sound(frequencies[5], 500);
        break;
      case 'h': 
        emit_sound(frequencies[6], 500);
        break;
    }
  }
}

void emit_sound(int frequency, int duration_ms) {
  float iteration_duration_s = (1.0 / frequency);

  float iterations = (duration_ms / 1000.0) / iteration_duration_s;
  Serial.println(iterations);

  while (0<iterations--) {
    emit_wave(frequency);
  }
}

void emit_wave(int frequency) {
  int value_duration = (1.0 / frequency / wave_length) * 1000000.0;

  for (int i = 0; i < wave_length; i++) {
    int value = wave_values[i] >> 6;

    PORTB = value;

    delayMicroseconds(value_duration);
  }
}
