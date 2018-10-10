unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, DateUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonInsertionSort: TButton;
    ButtonGenerate: TButton;
    ButtonBubbleSort: TButton;
    LabelIterations: TLabel;
    LabelTime: TLabel;
    StringGrid1: TStringGrid;
    procedure ButtonGenerateClick(Sender: TObject);
    procedure ButtonBubbleSortClick(Sender: TObject);
    procedure ButtonInsertionSortClick(Sender: TObject);

    procedure WriteToGrid(Arr: array of Integer; Grid: TStringGrid);
  private

  public

  end;

var
  Form1: TForm1;
  Numbers: array [0..20000] of Integer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonGenerateClick(Sender: TObject);
var
  i: Integer;
begin
     Randomize;

     for i := 0 to Length(Numbers) - 1 do begin
         { Fill with Integers from [0, 100000). }
         Numbers[i] := random(100000);
     end;

     WriteToGrid(Numbers, StringGrid1);
end;

procedure TForm1.ButtonBubbleSortClick(Sender: TObject);
var
  time_start: TDateTime;
  time_elapsed, iterations, outer_i, inner_i, tmp: Integer;
begin
     iterations := 0;
     time_start := Now;

     for outer_i := 0 to Length(Numbers) - 1 do begin
         { Set to zero to be able to detect bailout condition. }
         tmp := 0;

         for inner_i := 0 to Length(Numbers) - 1 do begin
             if Numbers[inner_i] > Numbers[inner_i + 1] then begin
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
  i, j, tmp, iterations: Integer;
begin
     i := 0;
     j := 0;
     iterations := 0;
     time_start := Now;

     {
     while i < Length(Numbers) do begin
         Inc(i);
         j := i;

         while (j > 0) and (Numbers[j - 1] > Numbers[j]) do begin
               tmp := Numbers[j];
               Numbers[j] := Numbers[j - 1];
               Numbers[j - 1] := tmp;

               Dec(j);
         end;
     end;
     }

     for i := 0 to Length(Numbers) do begin
         j := i;
         tmp := Numbers[i];

         while (j > 0) and (Numbers[j - 1] > tmp) do begin
             Numbers[j] := Numbers[j - 1];

             Dec(j);
         end;

         iterations := iterations + i - 1 - j;

         Numbers[j] := tmp;
     end;

     WriteToGrid(Numbers, StringGrid1);

     LabelIterations.Caption := IntToStr(iterations);
     LabelTime.Caption := IntToStr(MillisecondsBetween(time_start, Now))
                       + 'ms';
end;

{ Writes an array of Integers of variable size to a given StringGrid. Fails if
  the array does not fit into the grid. Moves horizontal first, from left to
  right and top to bottom. }
procedure TForm1.WriteToGrid(Arr: array of Integer; Grid: TStringGrid);
var
  i: Integer;
begin
     if Grid.ColCount * Grid.RowCount < Length(Arr) then
        Exit;

     for i := 0 to length(Arr) - 1 do begin
         Grid.cells[i mod 15, i div 15] := IntToStr(Arr[i]);
     end;
end;

end.

