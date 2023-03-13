unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    btAdd: TButton;
    btPDFFile: TButton;
    DBGrid1: TDBGrid;
    dsUsers: TDataSource;
    edName: TEdit;
    edEmail: TEdit;
    edPass: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mtUsers: TMemDataset;
    rgActive: TRadioGroup;
    procedure btAddClick(Sender: TObject);
    procedure btPDFFileClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  Dataset.PDF;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btAddClick(Sender: TObject);
begin
  if not mtUsers.Active then
    mtUsers.Active := True;
  mtUsers.Append;
  mtUsers.FieldByName('name').AsString := edName.Text;
  mtUsers.FieldByName('email').AsString := edEmail.Text;
  mtUsers.FieldByName('pass').AsString := edPass.Text;
  mtUsers.FieldByName('active').AsInteger := rgActive.ItemIndex;
  mtUsers.Post;
  edName.Clear;
  edEmail.Clear;
  edPass.Clear;
  rgActive.ItemIndex := -1;
  edName.SetFocus;
end;

procedure TForm1.btPDFFileClick(Sender: TObject);
begin
  mtUsers.ToPDFFile('file.pdf','user.str');
end;

end.

