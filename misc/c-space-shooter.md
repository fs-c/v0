# Space Shooter

In dieser Übung werden wir ein relativ fortgeschrittenes Computerspiel in C entwickeln, welches im Terminalemulator -- unter Windows standardmäßig, CMD -- läuft. Wir werden im Terminal mithilfe von speziellen control codes (Ketten von escape charactern) das gesamte Spiel zeichnen und durch Tastendrücke auf dem Keyboard kontrollieren.

Für diese Übung solltest du bereits einfache Programmierkentnisse haben, die Konzepte von Schleifen, Bedingungen und Assignments bzw. Variablen sollten nichts neues sein. Im Grunde genommen ist die Programmiersprache C eine sehr einfache, und viele andere Sprachen (z.B. JavaScript, Java, C#, ...) sind ihr oberflächlich ähnlich bzw. nachempfunden. Im folgenden werden daher keine näheren Details zu C gegeben -- sollte dir etwas unklar sein, zögere nicht dich selbst im Internet schlau zu machen, oder eine/n Mentor/in um Hilfe zu bitten.

## Spielablauf

![Screenshot während des pausierten Spiels](https://i.imgur.com/4TgnLB8.png)

Gegner, hier 4x4 Rechtecke, fliegen von oben nach unten und müssen vom Spieler abgeschossen werden. Wie im Luftkampf zwischen kleineren Fliegern üblich, reicht ein einziger Treffer um die feindlichen Rechtecke auszuschalten. Das Spiel läuft endlos, bis eines der gegnerischen Objekte das untere Ende des Bildschirms erreicht, wobei jeder Abschuss einen Punkt bringt -- das Ziel ist die Anhäufung möglichst vieler Punkte.

Der Spieler kontrolliert sein Raumschiff vertikal und horizontal (also von links nach rechts, und von oben nach unten) wie in Computerspielen üblich mit den WASD Tasten, und kann mit drücken der Space-Taste Geschosse aubfeuern.

![Screenshot zu Beginn des Spiels](https://i.imgur.com/p0jb5PI.png)

Dieser screenshot nimmt indirekt ein Implementationsdetail vorweg, die Bewegungspräzision. Um schnelle und flexible Bewegung zu erlauben, bewegt sich das Raumschiff normalerweise in Viererschritten: ein drücken der Taste 'A' == vier Einheiten nach links. Für genauere Bewegung und dadurch präzisere Schüsse, kann durch halten der Shift-Taste die Bewegung in Einserschritten eingeschalten werden.

Wenn du möchtest, kannst du dir das ausprogrammierte Spiel [hier](https://github.com/LW2904/vt-space/releases) herunterladen, um den Spielablauf genauer zu sehen.

TODO: Upload a build to the vt-space repo, might need some cleanup first

## Vorwissen

__VT100 Terminal Control Escape Sequences__ (kurz: VT100 codes) erlauben uns innerhalb eines Terminals bzw. Terminalemulators z.B. die Curserposition zu ändern, oder den Bildschirm zu löschen.

Escape sequences werden, wie escape character, zwar abgesendet (z.B. via `printf` in C oder `process.stdout.send` in NodeJS) aber vom Terminal nicht genau so ausgegeben. Der escape character `\n` gibt beispielsweise an, dass ein Text von einer Zeile in die nächste übergehen soll. Eine escape sequence ist ganz einfach eine Kette von Zeichen welche interpretiert, also nicht als solche ausgegeben, werden -- so löscht `\e[2J` z.B. den sichtbaren Bildschirm.

Eine Liste nützlicher escape sequences ist [hier](http://www.termsys.demon.co.uk/vtansi.htm) zu finden, weiter unten werden die jeweils wichtigen codes jedoch noch einmal aufgeführt.

Das folgende Beispiel demonstriert die Verwendung einer VT100 escape sequence (bzw. eines VT100 codes).

```C
// Untested

#include <stdio.h>

int main()
{
	printf("\e[2J");
}
```

Hier sind jedoch sind zwei "bad-practises" enthalten, also schlechter Code-Stil:
- `printf("\e[2J")` ist "Magie" -- es ist nicht direkt ersichtlich was dieses Stück code macht
- `\e` ist nicht standardisiert (also nicht garantiert das, was wir erwarten)

Eine schönere Lösung wäre daher

```C
// Untested

#include <stdio.h>

/* 0x1B is the ASCII "escape" character. */
#define ESC 0x1B

void clear_screen();

int main()
{
	clear_screen();
}

void clear_screen()
{
	printf("%c[2J", ESC);
}
```

Die Methode `clear_screen` abstrahiert in diesem Fall den "magischen" Teil, und ihre Funktion ist klar. Solche Abstraktionen sind im wesentlichen Vereinfachungen -- sie "verstecken" kompliziertere Aufgaben und Abläufe unter einem schnell verständlichen und lesbaren Namen, in diesem Fall `clear_screen`.

Zusätzlich wurde `\e` durch den Buchstaben mit dem code `0x1B` ersetzt, welcher für `ESC` steht. In vielen Systemen ist `\e` ein gültiger escape code für `0x1B`, jedoch ist er nicht (wie beispielsweise `\n`) standardisiert. Durch die verwendung des tatsächlichen codes können wir garantieren, dass unser Spiel in jedem Standardkonformen Terminal läuft.

Verstanden zu haben wie escape characters und sequences, und damit vt100 codes, zu verwenden sind, ist der Schlüssel zu dieser Übung. Wenn dir hier etwas unklar ist, solltest du dich noch ein wenig mit der obenstehenden Sektion beschäftigen, oder eine/n Mentor/in danach fragen.

## Projektsetup

TODO: Projektsetup Beschreibung -- MinGW oder VS? `conio.h` muss verfügbar sein!

TODO: Projektstruktur (build script? `make`?)

## Entwicklungsschritte

Bei größeren Projekten ist es immer hilfreich, die Entwicklung auf kleinere Schritte herunterzubrechen. In diesem Fall könnte das in etwa wie folgend aussehen:

- Die Höhe und Breite des Terminal-Fensters in Zeilen und Spalten (nicht pixel) ist bekannt
- Der Terminalemulator unterstützt VT100 Codes und asynchronen Input
- Das Raumschiff kann...
	- in verschiedenen Positionen gezeichnet werden
	- durch die WASD-Tasten bewegt werden, ...
		- allerdings nur innerhalb des Terminal-Fensters
		- mithilfe verschiedener Präzisionsstufen (z.B. WASD: 4er Intervall, Shift+WASD: 1er Intervall)
- Projektile können...
	- in verschiedenen Positionen gezeichnet werden
	- vom Raumschiff aus durch Drücken der Space-Taste "abgefeuert" werden, dabei...
		- fliegen sie immer "aus" der Schnauze des Raumschiffes heraus, und
		- behalten sie eine konstante, angebrachte Geschwindigkeit bei
	- eine gewisse Maximalanzahl nicht überschreiten
- Gegner...
	- können in verschiedenen Positionen gezeichnet werden
	- fliegen vom oberen Fensterrand zum unteren, und...
		- behalten eine konstante, angebrachte Geschwindigkeit bei 
	- erscheinen in einem langsam höher werdenen Intervall
	- verschwinden, wenn sie von einem Projektil "getroffen" werden
	- verursachen ein "Game Over" wenn sie den unteren Bildschirmrand erreichen

Diese Schritte können von oben nach unten durchgearbeitet werden, in den folgenden Abschnitten wird immer jeweils eine kurze Erläuterung der Probleme und Schwierigkeiten, sowie eine _potentielle_ Lösung gegeben sein. Versuche zuerst die Schritte ohne der "Lösung" zu bearbeiten, und verwende auch andere Ressourcen wie das Internet. Wichtig ist hierbei, dass das gegebene Codebeispiel auf keinen Fall das einzig richtige sein muss, es sollte nur als Hilfestellung dienen.

## Das Terminal-Fenster

### Höhe und Breite

Unser erster Schritt wird das Ermitteln der Höhe und Breite des Terminal-Fensters sein. Wir benötigen diese um
- sicherzustellen, dass sich unser Raumschiff nicht aus dem Fenster bewegt
- Projektile nach erreichen des oberen Endes verschwinden zu lassen
- festzustellen, ob ein Gegner das untere Ende erreicht hat
- Gegner entlang des oberen Endes erscheinen zu lassen

Informationen über die relevanten Methoden der Windows-API sind in den [Microsoft Docs unter "Window and Screen Buffer Size"](https://docs.microsoft.com/en-us/windows/console/window-and-screen-buffer-size) zu finden -- wichtig ist insbesondere die `GetConsoleScreenBufferInfo()` Methode, welche einen `HANDLE` zur Konsole erwartet.

Eine mögliche Lösung könnte wie folgt aussehen:

```C
// Untested, straight copy

int get_terminal_dimensions(int *columns, int *lines)
{
	DWORD access = GENERIC_READ | GENERIC_WRITE;
	DWORD mode = FILE_SHARE_READ | FILE_SHARE_WRITE;
	HANDLE console = CreateFileW(L"CONOUT$", access, mode, NULL,
		OPEN_EXISTING, 0, NULL);

	CONSOLE_SCREEN_BUFFER_INFO screen;
	if (!GetConsoleScreenBufferInfo(console, &screen))
		return -1;

    	*lines = screen.srWindow.Bottom - screen.srWindow.Top + 1;
	*columns = screen.srWindow.Right - screen.srWindow.Left + 1;

	return 0;
}
```

### VT100 Codes und asynchroner Input

Wir benötigen Unterstützung für VT100 primär um
- den cursor zu bewegen, und damit zu zeichnen
- den Bildschirm zu löschen

Letzteres ist hierbei ein vitaler Punkt: wir werden den Bildschirm mehrmals in der Sekunde löschen und die "Szene" neu zeichnen. Bei jeden Übergang von einer "Szene" (in diesem Kontext auch "Frame" genannt) in die nächste werden Änderungen wie zum Beispiel eine Bewegung des Raumschiffs oder eines Projektils sichtbar werden.

Hier wichtig sind die [`GetConsoleMode()`](https://docs.microsoft.com/en-us/windows/console/getconsolemode) und [`SetConsoleMode()`](https://docs.microsoft.com/en-us/windows/console/setconsolemode) Methoden der Windows-API. Mehr Informationen und Beispielcode können im Artikel ["Console Virtual Terminal Sequences"](https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences#example-of-enabling-virtual-terminal-processing) der Microsoft Docs gefunden werden.

Wenn du im oben verlinkten Artikel den Abschnitt "Example of Enabling Virtual Terminal Processing" gelesen hast, wirst du sehen, dass die folgende potentielle Lösung deutlich kürzer und anders als das dort gegebene Beispiel ist. Auch deshalb sei hier nochmals angemerkt, dass diese Lösungen bei weitem nicht die einzig richtigen (oder perfekt) sind -- hier wurde bewusst ein anderer Weg genommen, um dies zu verdeutlichen.

```C
// Untested, straight copy

int setup_terminal()
{
	DWORD access = GENERIC_READ | GENERIC_WRITE;
	DWORD mode = FILE_SHARE_READ | FILE_SHARE_WRITE;
	HANDLE console = CreateFileW(L"CONOUT$", access, mode, NULL,
		OPEN_EXISTING, 0, NULL);

	/* Fetch original console mode */
	if (!GetConsoleMode(console, &mode)) {
		printf("GetConsoleMode error: %ld\n", GetLastError());
		return -1;
	}

	/* Amend the mode to enable VT codes */
	mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;

	/* Apply the changes */
	if (!SetConsoleMode(console, mode)) {
		printf("SetConsoleMode error: %ld\n", GetLastError());
		return -1;
	}

	return 0;
}
```

Ebenso wichtig wie VT100-Unterstützung ist asynchroner input -- also input, auf den nicht gewartet wird. Üblicherweise wird der Programmablauf nach einem Aufruf von z.B. `getchar()` pausiert, bis der Benutzer einen Buchstaben sendet. Diese Art des Benutzerinputs wird auch "blocking input", oder "synchroner input" genannts. Asynchroner input ist am besten mit "non-blocking", also "nicht blockierender", input beschrieben.

Implementationen eines solchen sind von OS zu OS sehr unterschiedlich, unter Windows werden die Funktionen [`_getch`](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/getch-getwch?view=vs-2017) und [`_kbhit`](https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/kbhit?view=vs-2017) des `conio.h` headers hilfreich sein.

Mithilfe dieser zwei Funktionen kann eine sehr einfache implementation in etwa so aussehen:

```C
// Untested, straight copy

char getchar_nonblock()
{
	/* If a key was pressed just now... */
	if (_kbhit())
		/* ...return it */
		return _getch();
	
	return EOF;
}
```

Die oben stehende Funktion wird immer `EOF` (eine in der standard library definierte Konstante, meistens `-1`) zurückgeben, ausser wenn der Benutzer _gerade eben_ eine Taste gedrückt hat -- in diesem Fall gibt sie den gedrückten Buchstaben zurück.

Um also jede gedrückte Taste "echoen" zu lassen, also sie wieder auszugeben, könnte man die folgende Methode verwenden:

```C
// Untested

void echo_input()
{
	while ((char c = getchar_nonblock()))
		if (c != EOF)
			putchar(c);
}
```

Verständnisfrage: Wie könnte man mithilfe von `_kbhit()` und `_getch()` die C Standard Library Funktion `getchar()` implementieren?

```C
// Untested

char custom_getchar()
{
	while (!(_kbhit()))
		;

	return _getch();
}
```

### Fazit

Damit ist alles rund um den Terminalemulator (bzw. das Fenster ebenjenes) getan. Eine Implementation der bis jetzt eingeführten Methoden ist in der [`1-terminal` branch](https://github.com/LW2904/vt-space/tree/1-terminal) des `vt-space` Projekts zu finden.