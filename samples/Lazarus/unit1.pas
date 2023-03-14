unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, DB, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, DBGrids, libjpfpdf, libjpfpdfextends;

type

  { TForm1 }

  TForm1 = class(TForm)
    btAdd: TButton;
    btPDFFile: TButton;
    cbDefaultHeader: TCheckBox;
    cbDefaultFooter: TCheckBox;
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
    procedure CustomHeader(APDF: TJPFpdfExtends);
    procedure CustomFooter(APDF: TJPFpdfExtends);
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
  TDataSetPDFConfig.GetInstance.Title := 'Lista de Usuários';

  TDataSetPDFConfig.GetInstance.PrintDefaultHeader := cbDefaultHeader.Checked;
  if TDataSetPDFConfig.GetInstance.PrintDefaultHeader then
    TDataSetPDFConfig.GetInstance.OnDoHeader := nil
  else
    TDataSetPDFConfig.GetInstance.OnDoHeader := @CustomHeader;

  TDataSetPDFConfig.GetInstance.PrintDefaultFooter := cbDefaultHeader.Checked;
  TDataSetPDFConfig.GetInstance.PrintDefaultFooter := cbDefaultHeader.Checked;
  if TDataSetPDFConfig.GetInstance.PrintDefaultFooter then
    TDataSetPDFConfig.GetInstance.OnDoFooter := nil
  else
    TDataSetPDFConfig.GetInstance.OnDoFooter := @CustomFooter;
  mtUsers.ToPDFFile('file.pdf','user.str');
end;

procedure TForm1.CustomHeader(APDF: TJPFpdfExtends);
begin
  APDF.SetUTF8(True);
  APDF.SetFont(ffHelvetica, fsBold, 10);
  APDF.Cell(0,20,'Custom Header','0',0,'C');
  APDF.Ln(20);
end;

procedure TForm1.CustomFooter(APDF: TJPFpdfExtends);
begin
  APDF.SetUTF8(True);
  APDF.SetY(-12);
  APDF.SetFont(ffHelvetica,fsNormal,8);
  APDF.Cell(0,10,'Custom Footer','0',0,'C');
end;

end.

