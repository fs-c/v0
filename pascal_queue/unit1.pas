unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  Customer = record
           Plate: String;
           Next: ^Customer;
  end;

  ptrCustomer = ^Customer;

  { TForm1 }

  TForm1 = class(TForm)
    NewButton: TButton;
    PeekButton: TButton;
    CompleteButton: TButton;
    PlateEdit: TEdit;
    InfoLabel: TLabel;
    QueueMemo: TMemo;
    procedure CompleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure PeekButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }

    procedure Print();
  end;

  function Length(): Integer;
  function Pop(): ptrCustomer;
  procedure Push(Node: ptrCustomer);

  procedure Remove(Index: Integer);
  function Get(Index: Integer): ptrCustomer;

var
  Form1: TForm1;
  Queue, Last: ptrCustomer;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.NewButtonClick(Sender: TObject);
var
  Cstmr: ptrCustomer;
begin
  new(Cstmr);

  Cstmr^.Plate := PlateEdit.Text;

  Push(Cstmr);

  Print;
end;

procedure TForm1.PeekButtonClick(Sender: TObject);
begin
  if Queue = nil then begin
    InfoLabel.Caption := 'Queue is empty';

    Exit;
  end;

  InfoLabel.Caption := Queue^.Plate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Queue := nil;
  Last := nil;
end;

procedure TForm1.CompleteButtonClick(Sender: TObject);
var
  RemovedCstmr: ptrCustomer;
begin
  RemovedCstmr := Pop();

  dispose(RemovedCstmr);

  Print;
end;

procedure TForm1.Print();
var
  Len: Integer;
  Cstmr: ptrCustomer;
begin
  QueueMemo.Clear;

  Len := 0;
  Cstmr := Queue;
  while Cstmr <> nil do begin
      Inc(Len);

      QueueMemo.Append(Cstmr^.Plate);

      Cstmr := Cstmr^.Next;
  end;

  InfoLabel.Caption := 'Length: ' + IntToStr(Len);
end;

function Pop(): ptrCustomer;
var
  RemovedCstmr: ptrCustomer;
begin
  new(RemovedCstmr);

  RemovedCstmr^.Plate := Queue^.Plate;
  RemovedCstmr^.Next := Queue^.Next;

  result := RemovedCstmr;

  Remove(0);
end;

procedure Push(Node: ptrCustomer);
begin
  Node^.Next := nil;

  if Queue = nil then begin
    Queue := Node;
    Last := Node;
  end else begin
    Last^.Next := Node;
    Last := Node;
  end;
end;

function Get(Index: Integer): ptrCustomer;
var
  i: Integer;
  Node: ptrCustomer;
begin
  i := 0;
  Node := Queue;

  while (Node <> nil) and (i < index) do begin
      Inc(i);
      Node := Node^.Next;
  end;

  Exit(Node);
end;

function Length(): Integer;
var
  i: Integer;
  Node: ptrCustomer;
begin
  i := 0;
  Node := Queue;

  while (Node <> nil) do begin
      Inc(i);
      Node := Node^.Next;
  end;

  Exit(i);
end;

procedure Remove(Index: Integer);
var
  i: Integer;
  cur, prev: ptrCustomer;
begin
  cur := Queue;

  if Index = 0 then begin
    Queue := Queue^.Next;
    dispose(cur);
  end else begin
    i := 0;
    cur := Queue;
    prev := nil;

    while (cur <> nil) and (i < Index) do begin
        Inc(i);
        prev := cur;
        cur := cur^.Next;
    end;

    if cur <> nil then begin
      prev^.Next := cur^.Next;

      if prev^.Next = nil then
         Last := prev;

      dispose(cur);
    end;
  end;
end;

end.

