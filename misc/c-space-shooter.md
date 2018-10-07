# Space Shooter

In dieser Übung werden wir ein relativ fortgeschrittenes Computerspiel in C entwickeln, welches im Terminalemulator -- unter Windows standardmäßig, CMD -- läuft. Wir werden im Terminal mithilfe von speziellen control codes (Ketten von escape charactern) das gesamte Spiel zeichnen und durch Tastendrücke auf dem Keyboard kontrollieren.

Für diese Übung solltest du bereits einfache Programmierkentnisse haben, die Konzepte von Schleifen, Bedingungen und Assignments bzw. Variablen sollten nichts neues sein. Im Grunde genommen ist die Programmiersprache C eine sehr einfache, und viele andere Sprachen (z.B. JavaScript, Java, C#, ...) sind ihr oberflächlich ähnlich bzw. nachempfunden. Im folgenden werden daher keine näheren Details zu C gegeben -- sollte dir etwas unklar sein, zögere nicht dich selbst im Internet schlau zu machen, oder eine/n Mentor/in um Hilfe zu bitten.

## Spielablauf

PLACEHOLDER:
![Screenshot während des pausierten Spiels](https://i.imgur.com/4TgnLB8.png)

Gegner, hier 3x4 Rechtecke, fliegen von oben nach unten und müssen vom Spieler abgeschossen werden. Wie im Luftkampf zwischen kleineren Fliegern üblich, reicht ein einziger Treffer um die feindlichen Rechtecke auszuschalten. Das Spiel läuft endlos, bis eines der gegnerischen Objekte das untere Ende des Bildschirms erreicht, wobei jeder Abschuss einen Punkt bringt -- das Ziel ist die Anhäufung möglichst vieler Punkte.

Der Spieler kontrolliert sein Raumschiff vertikal und horizontal (also von links nach rechts, und von oben nach unten) wie in Computerspielen üblich mit den WASD Tasten, und kann mit drücken der Space-Taste Geschosse aubfeuern.

PLACEHOLDER:
![Screenshot zu Beginn des Spiels](https://i.imgur.com/p0jb5PI.png)

Dieser screenshot nimmt indirekt ein Implementationsdetail vorweg, die Bewegungspräzision. Um schnelle und flexible Bewegung zu erlauben, bewegt sich das Raumschiff normalerweise in Achterschritten: ein drücken der Taste 'A' == acht Einheiten nach links. Für genauere Bewegung und dadurch präzisere Schüsse, kann durch halten der Shift-Taste die Bewegung in Zweierschritten eingeschalten werden.

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
		- mithilfe verschiedener Präzisionsstufen (z.B. WASD: 8er Intervall, Shift+WASD: 2er Intervall)
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

## Das Raumschiff

### Zeichnen

Zum zeichnen des Raumschiffs werden wir eine `draw_ship` Methode entwickeln, welche nur wissen muss wo sie das Schiff zeichnen soll. Optional könnte man benutzerdefinierte Raumschiff-Dimensionen implementieren, aber dafür gibt es in diesem Fall wenig Verwendung.

Alle gezeichneten Objekte bestehen auf dem niedrigsten Level aus einzelnen Punkten. Um einen Punkt auf einer position P zu zeichnen, muss

- der Cursor auf die Position P bewegt werden, und
- ein möglichst deckender Buchstabe (wie `#`) ausgegeben werden

Um den Cursor zu bewegen gibt es die VT100 Sequence `<ESC> [<line>;<column> H` (whitespace wird beim parsen der Sequenzen ignoriert). Neu ist bei diesem Code, dass wir Parameter, separiert durch Strichpunkte, übergeben.

Das schreit nach einer Abstraktion, zum Beispiel wie folgend:

```C
// Untested

void move_cursor(int x, int y)
{
	/* Note the argument order since this expects line, column (ergo y, x)
	   instead of the more common x, y. */
	printf("%c [ %d;%d H", ESC, y, x);
}
```

Jetzt wo `move_cursor` implementiert ist, können wir uns an die `draw_dot` Methode machen. Ihre Funktionsweise wurde oben bereits erläutert, hier eine mögliche Implementation

```C
// Untested

#define DRAW_CHAR '#'

void draw_dot(int x, int y)
{
	move_cursor(x, y);

	putchar(DRAW_CHAR);
}
```

Damit ist die `draw_ship` Methode quasi schon fertig, das Design des Fliegers ist dir überlassen. Wie immer folgt natürlich ein Beispiel, bei dem auch gleich eine `draw_rectangle` Methode implementiert wurde.

```C
// Untested

#define PLAYER_WIDTH 3
#define PLAYER_HEIGHT 4

void draw_ship(int x, int y)
{
	/* Main body */
	draw_rectangle(x, y, PLAYER_WIDTH, PLAYER_HEIGHT);

	/* Snout */
	draw_dot(x + (PLAYER_WIDTH / 2), y - 1);

	/* Left wing */
	draw_dot(x - 1, y + 1);
	draw_dot(x - 1, y + 2);

	draw_dot(x - 2, y + 2);
	draw_dot(x - 3, y + 2);

	/* Right wing */
	draw_dot(x + PLAYER_WIDTH, y + 1);
	draw_dot(x + PLAYER_WIDTH, y + 2);

	draw_dot(x + PLAYER_WIDTH + 1, y + 2);
	draw_dot(x + PLAYER_WIDTH + 2, y + 2);
}

void draw_rectangle(int x, int y, int width, int height)
{
	for (int rx = 0; rx < width; rx++)
		for (int ry = 0; ry < height; ry++)
			draw_dot(x + rx, y + ry);
}
```

### Bewegen

Wie bereits weiter oben erwähnt, werden wir das Terminal-Fenster mehrmals in der Sekunde löschen und neu zeichnen. Das heißt wir werden keine `move_ship_left` oder `move_ship_up` Methoden haben, sondern nur `run_frame`. (Wir erinnern uns: "Frame" beudeutet in diesem Kontext das Selbe wie "Szene".)

"Run" und nicht "draw", weil diese Methode mehr machen muss als nur Zeichen -- also `draw_*` -- Methoden zu kontrollieren, unter Anderem muss sie auch festgestellt werden, ob der Benutzer eine Taste (genauer, WASD oder Space) gedrückt hat. Tatsächlich wird später die gesamte Logik des Spieles durch diese Methode kontrolliert werden.

Aber nun zur Bewegung des Schiffes. `run_frame` muss vorerst...

- asynchron Input abfragen, ...
	- ...und basierend darauf die Position des Raumschiffs anpassen
- das Raumschiff in der angepassten Position zeichnen

Du solltest bis jetzt alle Bausteine die für die Implementation dieser Methode benötigt werden bereits ausgearbeitet haben -- der folgende Beispielcode ist entsprechend einfach, da die meiste Logik in bereits implementierte Methoden ausgelagert wird.

```C
// Untested

#define MOVEMENT_INTERVAL_SMALL 2
#define MOVEMENT_INTERVAL_DEFAULT 8

int term_w, term_h;
int player_x, player_y;

/* Fetch term_w and term_h, initialise player_x and player_y to sensible
   defaults. */

void run_frame()
{
	char c = getchar_nonblock();

	handle_player(c);
}

void handle_player(char c)
{
	/* If it's uppercase (ergo shift was held), use the small interval,
	   else use default. */
	int interval = c > 64 && c < 91 ? MOVEMENT_INTERVAL_SMALL :
		MOVEMENT_INTERVAL_DEFAULT;

	switch (c) {
	case 'w':
	case 'W': player_y -= interval;
		break;
	case 'a':
	case 'A': player_x -= interval;
		break;
	case 's':
	case 'S': player_y += interval;
		break;
	case 'd':
	case 'D': player_x += interval;
		break;
	}

	player_x = wrap_around(player_x, 0, term_w);
	player_y = wrap_around(player_y, 0, term_h);

	draw_ship(player_x, player_y);
}
```

Ein wichtiges Detail ist hierbei die Auslagerung aller Raumschiff-spezifischen Logik in eine `handle_player` Methode, welche (falls vorhanden) den gedrückten Buchstaben als Parameter bekommt. Dies wird später hilfreich werden, wenn andere Spielelemente in `run_frame` ausgeführt und kontrolliert werden müssen.

Diese Version der `handle_player` Methode erwartet, dass die globalen Variablen `player_x`, `player_y`, `term_w`, und `term_h` existieren und __intialisiert sind__. Die Verwendung der Methode könnte daher in etwa so aussehen:

```C
// Untested

/* ... */

int term_w, term_h;
int player_x, player_y;

int main()
{
	/* ... */

	get_terminal_dimensions(&term_w, &term_h);

	/* Start in the center and near the bottom of the screen */
	player_x = term_w * 0.5;
	player_y = term_h * 0.8;

	while (1) {
		run_frame();

		/* Sleep for 0.5s */
		Sleep(500);
	}
}

/* ... */
```

Ein wichtiges Detail welches in der `handle_player` Methode noch nicht implementiert wurde, ist die Einschränkung der Bewegung in das Terminal-Fenster. Dies kann auf verschiedenste Wege gelöst werden, im folgenden Beispiel wird eine `wrap_around` Methode verwendet um das Schiff aus dem jeweilig gegenüberliegendem Rand "hereinfliegen" zu lassen, sollte ein Rand überschritten werden.

```C
// Untested

void handle_player(char c)
{
	/* ... */

	/* Allow infinite movement by looping the player back around if they
	   move over one of the edges. */
	player_x = wrap_around(player_x, 0, term_w);
	player_y = wrap_around(player_y, 0, term_h);

	draw_ship(player_x, player_y);
}

int wrap_around(int actual, int min, int max)
{
	if (actual < min)
		return (min - actual) - max;
	if (actual > max)
		return (actual - max) + min;
	
	return actual;
}
```

### Fazit

Damit ist die rudimentäre Bewegung des Raumschiffes abgeschlossen. Wir werden noch viel mit der `run_frame` Methode arbeiten, aber der Rahmen und wohl wichtigste Teil des Spieles, steht.

Wie auch schon zuvor, ist eine Implementation der neuen Methoden in der [`2-spaceship` branch](https://github.com/LW2904/vt-space/tree/2-spaceship) zu finden.

## Projektile und Schießen

### Zeichnen

Unsere Geschosse werden einfache Linien sein, in etwa wie kurze Lasersalven. Nachdem wir ja bereits eine `draw_rectangle` Methode entwickelt haben, ist die Implementation einer `draw_projectile` Methode eine einfache Aufgabe.

```C
void draw_projectile(int x, int y)
{
	const int width = 1;
	const int height = 4;

	draw_rectangle(x, y, width, height);
}
```

### Abfeuern

Um Projektile abfeuern zu können müssen wir

- wissen ob gerade Space gedrückt wurde, um festzustellen ob ein neues Projektil hinzugefügt werden soll
- alle Projektile die gerade im Flug sind kennen, insbesondere...
	- ihre Position um sie...
		- nach oben "fliegen" zu lassen
		- zu entfernen, sollten sie den oberen Rand erreicht haben
	- ihre Geschwindigkeit

Die Geschwindigkeit wird sich während des Fluges nicht ändern, und wir werden sie als "Zeilen/Frame" definieren. Ein Projektil mit einer Geschwindigkeit von 3 wird pro Frame drei Zeilen nach oben fliegen, also um drei Zeilen nach oben bewegt werden.

Wie zuvor mit `handle_player` werden wir auch hier eine `handle_projectiles` Methode implementieren, die für jedes Frame aufgerufen wird.

```C
#define MAX_PROJECTILES 16

struct projectile {
	int x;
	int y;
	int speed;
}

int num_projectiles = 0;
struct projectiles *projectiles = NULL;

/* Allocate memory for the the maximum number of projectiles. */

void handle_projectiles(char c)
{
	if (c == ' ') {
		projectiles[num_projectiles++] = (struct projectile){
			player_x + PLAYER_WIDTH / 2, player_y - 1,
			PROJECTILE_SPEED
		};
	}

	/* Remove oldest projectile(s) if there are too many. */
	for (int i = 0; i <= num_projectiles - MAX_PROJECTILES; i++) {
		remove_projectile(i);
	}

	for (int i = 0; i < num_projectiles; i++) {
		handle_projectile(i);
	}
}

void handle_projectile(int index)
{
	struct projectile *p = projectiles + index;

	p->y -= p->speed;

	/* If the projectile has flown outside of the terminal... */
	if (p->y <= 0) {
		/* ...remove it. */
		remove_projectile(index);
	}
}

void remove_projectile(int index)
{
	remove_array_item(projectiles, index, num_projectiles--,
		sizeof(struct projectile));
}
```

Der obige Beispielcode ist relativ komplex, weshalb wir ihn hier noch einmal Schritt für Schritt durchgehen.

`void handle_projectiles(char c)`: 

```C
if (c == ' ') {
	projectiles[num_projectiles++] = (struct projectile){
		player_x + PLAYER_WIDTH / 2, player_y - 1,
		PROJECTILE_SPEED
	};
}
```

Wenn die gedrückte Taste (ergo `c`) gleich `' '` ist, also die Leertase war, füge an der Stelle `num_projectiles` ein neues `struct projectile` (ergo ein Projektil) hinzu, und erhöhe `num_projectiles` um eins. Das funktioniert, weil `num_projectiles`, also die Länge des `projectiles` Array, am Anfang `0` ist. Der erste Index auf den wir schreiben ist also `0`, und nachdem wir das neue Projektil hinzugefügt haben ist er `num_projectiles + 1` -> `0 + 1` -> `1`. Beim nächsten feuern, wird `num_projectiles` eins sein, also werden wir auf den Index `1` schreiben, und danach `num_projectiles` auf `2` erhöhen, und so weiter.

Das geht aber nur so lange gut, bis wir die maximale Länge erreicht haben -- in diesem Fall `MAX_PROJECTILES`, also 16. Würde diese Überschritten werden, würden wir in Speicherregionen schreiben die uns nicht gehören was zu sehr schwer findbaren Bugs führen würde. Die folgende Schleife soll dies verhindern.

```C
/* Remove oldest projectile(s) if there are too many. */
for (int i = 0; i <= num_projectiles - MAX_PROJECTILES; i++) {
	remove_projectile(i);
}
```

Bis `num_projectiles` einmal `MAX_PROJECTILES` erreicht hat, passiert hier nichts -- es gibt ja auch keinen Handlungsbedarf, genug Platz ist vorhanden. Erreicht `num_projectiles` nun aber `MAX_PROJECTILES` (oder überschreitet es gar), werden die ältesten (niedrigsten) Projektile entfernt, um Platz für das nächste zu machen.



Das könnte man so verdeutlichen:

```
...

num_projectiles = 15
-> Schleife läuft nicht, `i` (welches bei null anfängt) ist nicht kleiner oder gleich 15 - 16, also -1

<projectil wird hinzugefügt, num_projectiles wird um eins erhöht>

num_projectiles = 16
-> Schleife läuft einmal da `16 - 16 == 0`, und `0 <= 0`
--> Projektil an der Stelle null (also das Älteste) wird entfernt, num_projectiles ist nun wieder 15

<projectil wird hinzugefügt, num_projectiles wird um eins erhöht>

num_projectiles = 16
-> Schleife läuft einmal da...
--> ...

...
```

Man könnte die obenstehende `for` Schleife als

```C
if (num_projectiles == MAX_PROJECTILES) {
	num_projectiles = remove_projectile(num_projectiles - 1);
}
```

vereinfachen, würde dabei allerdings nur eine von (theorethisch) vielen Möglichkeiten abdecken. Praktisch wird `num_projectiles` wahrscheinlich nie höher als 16 sein -- sollte dies aber warum auch immer doch so sein, wird die Schleifen-Implementation noch immer in der Lage sein wie erwartet zu funktionieren.