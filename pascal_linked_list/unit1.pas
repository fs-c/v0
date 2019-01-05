unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  Person = record
    Name: string;
    Next: ^Person;
  end;

  ptrPerson = ^Person;

  TForm1 = class(TForm)
    AppendButton: TButton;
    LengthButton: TButton;
    GetByNameButton: TButton;
    SearchEdit: TEdit;
    GetByIndexButton: TButton;
    OutputLabel: TLabel;
    NameEdit: TEdit;
    ListMemo: TMemo;
    procedure AppendButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GetByIndexButtonClick(Sender: TObject);
    procedure GetByNameButtonClick(Sender: TObject);
    procedure LengthButtonClick(Sender: TObject);
    procedure Print();
  private
    { private declarations }
  public
    { public declarations }
  end;

  procedure Append(Node: ptrPerson);

  function Length(): Integer;

  function Get(Name: String): Integer;
  function Get(Index: Integer): ptrPerson;

var
  Form1: TForm1;
  List, Last: ptrPerson;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AppendButtonClick(Sender: TObject);
var
  Node: ptrPerson;
begin
  new(Node);

  Node^.Name := NameEdit.Text;

  Append(Node);

  Print;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  List := nil;
end;

procedure TForm1.GetByIndexButtonClick(Sender: TObject);
begin
  OutputLabel.Caption := 'Name: ' + Get(StrToInt(SearchEdit.Text))^.Name;
end;

procedure TForm1.GetByNameButtonClick(Sender: TObject);
begin
  OutputLabel.Caption := 'Index: ' + IntToStr(Get(SearchEdit.Text));
end;

procedure TForm1.LengthButtonClick(Sender: TObject);
begin
  OutputLabel.Caption := 'Length: ' + IntToStr(Length);
end;

procedure TForm1.Print();
var
  Node: ptrPerson;
begin
  ListMemo.Clear;

  Node := List;

  while Node <> nil do begin
      ListMemo.Append(Node^.Name);

      Node := Node^.Next;
  end;
end;

procedure Append(Node: ptrPerson);
begin
  Node^.Next := nil;

  if List = nil then begin
    List := Node;
    Last := Node;
  end else begin
    Last^.Next := Node;
    Last := Node;
  end;
end;

function Get(Index: Integer): ptrPerson;
var
  i: Integer;
  Node: ptrPerson;
begin
  i := 0;
  Node := List;

  while (Node <> nil) and (i < index) do begin
      Inc(i);
      Node := Node^.Next;
  end;

  Exit(Node);
end;

function Get(Name: String): Integer;
var
  i: Integer;
  Node: ptrPerson;
begin
  i := 0;
  Node := List;

  while (Node <> nil) and (Node^.Name <> Name) do begin
        Inc(i);
        Node := Node^.Next;
  end;

  if Node = nil then
     Exit(-1);

  Exit(i);
end;

function Length(): Integer;
var
  i: Integer;
  Node: ptrPerson;
begin
  i := 0;
  Node := List;

  while (Node <> nil) do begin
        Inc(i);
        Node := Node^.Next;
  end;

  Exit(i);
end;

end.

