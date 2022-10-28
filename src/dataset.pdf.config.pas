unit Dataset.PDF.Config;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, libjpfpdf, libjpfpdfextends;

type

  { TDataSetPDFConfig }

  TDataSetPDFConfig = class
  private
    FFont: TPDFFontFamily;
    FFontStyle: TPDFFontStyle;
    FFontSize: Double;
    FOrientation: TPDFOrientation;
    FPageUnit: TPDFUnit;
    FPageFormat: TPDFPageFormat;
    FAddPage: Boolean;
    FOnDoHeader: TPDFEvent;
    FOnDoFooter: TPDFEvent;
    FPrintDefaultHeader: Boolean;
    FPrintDefaultFooter: Boolean;
    FTitle: String;

    class var FInstance: TDataSetPDFConfig;
  protected
    class function GetDefaultInstance: TDataSetPDFConfig;
  public
    constructor Create;

    property Font: TPDFFontFamily read FFont write FFont;
    property FontStyle: TPDFFontStyle read FFontStyle write FFontStyle;
    property FontSize: Double read FFontSize write FFontSize;
    property Orientation: TPDFOrientation read FOrientation write FOrientation;
    property PageUnit: TPDFUnit read FPageUnit write FPageUnit;
    property PageFormat: TPDFPageFormat read FPageFormat write FPageFormat;
    property AddPage: Boolean read FAddPage write FAddPage;
    property Title: String read FTitle write FTitle;
    property PrintDefaultHeader: Boolean read FPrintDefaultHeader write FPrintDefaultHeader;
    property PrintDefaultFooter: Boolean read FPrintDefaultFooter write FPrintDefaultFooter;

    property OnDoHeader: TPDFEvent read FOnDoHeader write FOnDoHeader;
    property OnDoFooter: TPDFEvent read FOnDoFooter write FOnDoFooter;

    class function GetInstance: TDataSetPDFConfig;
    class procedure UnInitialize;
  end;

implementation

{ TDataSetPDFConfig }

class function TDataSetPDFConfig.GetDefaultInstance: TDataSetPDFConfig;
begin
  if not Assigned(FInstance) then
    FInstance := TDataSetPDFConfig.Create;

  Result := FInstance;
end;

constructor TDataSetPDFConfig.Create;
begin
  FFont := ffHelvetica;
  FFontStyle := fsNormal;
  FFontSize := 10;
  FOrientation := poLandscape;
  FPageUnit := puMM;
  FPageFormat := pfA4;
  FAddPage := True;
  FTitle := EmptyStr;
  FPrintDefaultHeader := True;
  FPrintDefaultFooter := True;
end;

class function TDataSetPDFConfig.GetInstance: TDataSetPDFConfig;
begin
  Result := TDataSetPDFConfig.GetDefaultInstance;
end;

class procedure TDataSetPDFConfig.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

end.

