unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, DateUtils, LazLogger;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonBinarySearch: TButton;
    ButtonShellSort: TButton;
    ButtonInsertionSort: TButton;
    ButtonGenerate: TButton;
    ButtonBubbleSort: TButton;
    EditSearchValue: TEdit;
    LabelSearchComparisons: TLabel;
    LabelSearchIndex: TLabel;
    LabelIterations: TLabel;
    LabelTime: TLabel;
    StringGrid1: TStringGrid;
    procedure ButtonBinarySearchClick(Sender: TObject);
    procedure ButtonGenerateClick(Sender: TObject);
    procedure ButtonBubbleSortClick(Sender: TObject);
    procedure ButtonInsertionSortClick(Sender: TObject);
    procedure ButtonShellSortClick(Sender: TObject);

    procedure WriteToGrid(Arr: array of integer; Grid: TStringGrid);
    procedure BinarySearchRecursive(Value: integer; Min: integer; Max: integer; Hops: integer);
  private

  public

  end;

var
  Form1: TForm1;
  Numbers: array [0..20000] of integer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonGenerateClick(Sender: TObject);
var
  i: integer;
begin
  Randomize;

  for i := 0 to Length(Numbers) - 1 do
  begin
    { Fill with Integers from [0, 100000). }
    Numbers[i] := random(100000);
  end;

  WriteToGrid(Numbers, StringGrid1);
end;

procedure TForm1.ButtonBinarySearchClick(Sender: TObject);
begin
  BinarySearchRecursive(StrToInt(EditSearchValue.Text), 0, length(Numbers) - 1, 0);
end;

procedure TForm1.BinarySearchRecursive(Value: integer; Min: integer; Max: integer;
  Hops: integer);
var
  mid: integer;
begin
  if max >= min then begin
    mid := min + ((max - min) div 2);

    if Numbers[mid] < value then
       BinarySearchRecursive(value, mid + 1, max, Hops + 1);

    if Numbers[mid] > value then
       BinarySearchRecursive(value, min, mid - 1, Hops + 1);

    if Numbers[mid] = value then begin
       LabelSearchIndex.Caption := IntToStr(mid);
       LabelSearchComparisons := IntToStr(hops);
    end;
  end else begin
    LabelSearchIndex.Caption := 'Value not found';
    LabelSearchComparisons := IntToStr(hops);
  end;
end;

procedure TForm1.ButtonBubbleSortClick(Sender: TObject);
var
  time_start: TDateTime;
  time_elapsed, iterations, outer_i, inner_i, tmp: integer;
begin
  iterations := 0;
  time_start := Now;

  for outer_i := 0 to Length(Numbers) - 1 do
  begin
    { Set to zero to be able to detect bailout condition. }
    tmp := 0;

    for inner_i := 0 to Length(Numbers) - 1 do
    begin
      if Numbers[inner_i] > Numbers[inner_i + 1] then
      begin
        { Swap values. }
        tmp := Numbers[inner_i];
        Numbers[inner_i] := Numbers[inner_i + 1];
        Numbers[inner_i + 1] := tmp;
      end;
    end;

    iterations := iterations + ++inner_i;

    { If we never swapped values in this run, break out. }
    if tmp = 0 then
      break;
  end;

  WriteToGrid(Numbers, StringGrid1);

  time_elapsed := MillisecondsBetween(time_start, Now);

  LabelIterations.Caption := IntToStr(iterations);
  LabelTime.Caption := IntToStr(time_elapsed) + 'ms';
end;

procedure TForm1.ButtonInsertionSortClick(Sender: TObject);
var
  time_start: TDateTime;
  i, j, tmp, iterations: integer;
begin
  i := 0;
  j := 0;
  iterations := 0;
  time_start := Now;

  for i := 0 to Length(Numbers) do
  begin
    j := i;
    tmp := Numbers[i];

    while (j > 0) and (Numbers[j] > tmp) do
    begin
      Numbers[j] := Numbers[j - 1];

      Dec(j);
    end;

    iterations := iterations + i - 1 - j;

    Numbers[j] := tmp;
  end;

  WriteToGrid(Numbers, StringGrid1);

  LabelIterations.Caption := IntToStr(iterations);
  LabelTime.Caption := IntToStr(MillisecondsBetween(time_start,
    Now)) + 'ms';
end;

procedure TForm1.ButtonShellSortClick(Sender: TObject);
var
  i, pos, val, interval: integer;
begin
  interval := 1;

  while interval < (Length(Numbers) div 3) do
    interval := (interval * 3) + 1;

  while interval > 0 do begin
    for i := interval to Length(Numbers) do begin
      pos := i;
      val := Numbers[i];

      while (pos > interval) and (Numbers[pos - interval] >= val) do begin
        Numbers[pos] := Numbers[pos - interval];
        pos := pos - interval;
      end;

      Numbers[pos] := val;
    end;

    interval := (interval div 3) - 1;
  end;

  WriteToGrid(Numbers, StringGrid1);
end;

{ Writes an array of Integers of variable size to a given StringGrid. Fails if
  the array does not fit into the grid. Moves horizontal first, from left to
  right and top to bottom. }
procedure TForm1.WriteToGrid(Arr: array of integer; Grid: TStringGrid);
var
  i: integer;
begin
  if Grid.ColCount * Grid.RowCount < Length(Arr) then
    Exit;

  for i := 0 to length(Arr) - 1 do
  begin
    Grid.cells[i mod 15, i div 15] := IntToStr(Arr[i]);
  end;
end;

end.
