unit Dataset.PDF.Export;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, DB, DJPDFReport, libjpfpdf, libjpfpdfextends;

type

  { TDataSetPDF }

  TDataSetPDF = class
  private
    FDataSet: TDataSet;
    FDownloadFile: Boolean;
    FChildRecord: Boolean;
    FPDF: TDJPDFReport;
    FFileFieldStructure: String;

    function StrToAlignment(const AAlignment: String; const ADefaultAlignment: TAlignment): TAlignment;
    procedure LoadFieldStructure;
    procedure DoTableTitles(const ADataSet: TDataSet);
    procedure DoTableRows(const ADataSet: TDataSet);
    procedure ExportToPDF(ADataSet: TDataSet; var AHasChild: Boolean; const AChildRecords: Boolean; const ADrawTitle: Boolean);

    procedure DefaultHeader(APDF: TJPFpdfExtends);
    procedure DefaultFooter(APDF: TJPFpdfExtends);
  public
    constructor Create(const ADataSet: TDataSet; const AFileFieldStructure: String; const ADownloadFile: Boolean = True; const AChildRecords: Boolean = True);
    destructor Destroy; override;

    function ToStream: TStream;
  end;

implementation

uses
  DataSet.PDF.Config, Generics.Collections, DataSet.Serialize.Utils, fpjson, LConvEncoding, StrUtils;

{ TDataSetPDF }

function TDataSetPDF.StrToAlignment(const AAlignment: String;
  const ADefaultAlignment: TAlignment): TAlignment;
begin
  case AnsiIndexText(AAlignment, ['taleftjustify', 'tarightjustify', 'tacenter']) of
    0: Result := taLeftJustify;
    1: Result := taRightJustify;
    2: Result := taCenter;
  else
    Result := ADefaultAlignment;
  end;
end;

procedure TDataSetPDF.LoadFieldStructure;
var
  I: Integer;
  LFile: TStringList;
  LJSONFieldStructure: TJSONArray;
  LJSONObject : TJSONObject;
begin
  if (Trim(FFileFieldStructure) = EmptyStr) then
    Exit;

  if not FileExists(FFileFieldStructure) then
    Exit;

  LFile := TStringList.Create;
  try
    LFile.LoadFromFile(FFileFieldStructure);
    try
      LJSONFieldStructure := TJSONArray(GetJSON(LFile.Text));
    except
      On E: Exception do
        raise Exception.Create('Invalid Field Structure - '+E.Message);
    end;

    for I := 0 to Pred(LJSONFieldStructure.Count) do
    begin
      LJSONObject := LJSONFieldStructure.Items[I] as TJSONObject;
      if Assigned(FDataSet.FindField(LJSONObject.Get('fieldName',''))) then
      begin
        with FDataSet.FieldByName(LJSONObject.Get('fieldName','')) do
        begin
          DisplayWidth := LJSONObject.Get('displayWidth',DisplayWidth);
          DisplayLabel := LJSONObject.Get('displayLabel',DisplayLabel);
          Visible      := LJSONObject.Get('visible',Visible);
          Alignment    := StrToAlignment(LowerCase(LJSONObject.Get('alignment','')),Alignment);
        end;
      end;
    end;
  finally
    LFile.Free;
    LJSONFieldStructure.Free;
  end;
end;

procedure TDataSetPDF.DoTableTitles(const ADataSet: TDataSet);
var
  I: Integer;
  LField: TField;
  LArrayOfWidth: Array of Double = [];
  LArrayOfName: Array of String = [];
  LArrayOfAlignment: Array of TTexBoxAlignment = [];
begin
  for I:=0 to ADataSet.FieldCount-1 do
  begin
    LField := ADataSet.Fields[I];
    SetLength(LArrayOfWidth, I+1);
    SetLength(LArrayOfName, I+1);
    SetLength(LArrayOfAlignment, I+1);
    LArrayOfWidth[I] := LField.DisplayWidth;
    LArrayOfName[I] := LField.DisplayLabel;
    LArrayOfAlignment[I] := FPDF.ConvertTAlignmentToTTexBoxAlignment(LField.Alignment);
  end;
  FPDF.SetTableWidths(LArrayOfWidth);
  FPDF.SetTableAligns(LArrayOfAlignment);
  FPDF.SetTableTitles(LArrayOfName, True, True);

  FPDF.DrawTableTitles;
end;

procedure TDataSetPDF.DoTableRows(const ADataSet: TDataSet);
var
  I: Integer;
  LField: TField;
  LArrayOfTableRow: Array of String = [];
begin
  for I:=0 to ADataSet.FieldCount-1 do
  begin
    LField := ADataSet.Fields[I];
    SetLength(LArrayOfTableRow, I+1);
    LArrayOfTableRow[I] := CP1252ToUTF8(LField.Text);
  end;

  FPDF.TableRow(LArrayOfTableRow);
end;

procedure TDataSetPDF.ExportToPDF(ADataSet: TDataSet; var AHasChild: Boolean;
  const AChildRecords: Boolean; const ADrawTitle: Boolean);
var
  LDataSetDetails: TList<TDataSet>;
  LNestedDataSet: TDataSet;
  I: Integer = 0;
  LHasChild: Boolean = False;
begin
  if ADrawTitle then
    DoTableTitles(ADataSet);
  DoTableRows(ADataSet);

  if AChildRecords then
  begin
    LDataSetDetails := TList<TDataSet>.Create;
    try
      TDataSetSerializeUtils.GetDetailsDatasets(ADataSet, LDataSetDetails);
      for LNestedDataSet in LDataSetDetails do
      begin
        if (LNestedDataSet.RecordCount > 0) then
        begin
          AHasChild := True;
          FPDF.LnBreak(3);
          while not LNestedDataSet.EOF do
          begin
            ExportToPDF( LNestedDataSet, LHasChild, True, (I = 0));
            LNestedDataSet.Next;
            Inc(I);
          end;
        end;
      end;
    finally
      LDataSetDetails.Free;
    end;
  end;
end;

procedure TDataSetPDF.DefaultHeader(APDF: TJPFpdfExtends);
begin
  if (TDataSetPDFConfig.GetInstance.Title <> EmptyStr) then
  begin
    APDF.SetUTF8(True);
    APDF.SetFont(ffHelvetica, fsBold, 10);
    APDF.Cell(0,20,TDataSetPDFConfig.GetInstance.Title,'0',0,'C');
    APDF.Ln(20);
  end;
end;

procedure TDataSetPDF.DefaultFooter(APDF: TJPFpdfExtends);
begin
  APDF.SetUTF8(True);
  APDF.SetY(-12);
  APDF.SetFont(ffHelvetica,fsNormal,8);
  APDF.Cell(0,10,DateTimeToStr(Now),'0',0,'L');
  APDF.Cell(0,10,IntToStr(APDF.PageNo) + '/{nb}','0',0,'R');
end;

constructor TDataSetPDF.Create(const ADataSet: TDataSet;
  const AFileFieldStructure: String; const ADownloadFile: Boolean;
  const AChildRecords: Boolean);
begin
  FDataSet := ADataSet;
  FFileFieldStructure := AFileFieldStructure;
  FDownloadFile := ADownloadFile;
  FChildRecord := AChildRecords;
  FPDF := TDJPDFReport.Create(TDataSetPDFConfig.GetInstance.Orientation,
                              TDataSetPDFConfig.GetInstance.PageUnit,
                              TDataSetPDFConfig.GetInstance.PageFormat,
                              False);

  if Assigned(TDataSetPDFConfig.GetInstance.OnDoHeader) then
    FPDF.CorePDF.OnDoHeader := TDataSetPDFConfig.GetInstance.OnDoHeader
  else if TDataSetPDFConfig.GetInstance.PrintDefaultHeader then
    FPDF.CorePDF.OnDoHeader := DefaultHeader;

  if Assigned(TDataSetPDFConfig.GetInstance.OnDoFooter) then
    FPDF.CorePDF.OnDoFooter := TDataSetPDFConfig.GetInstance.OnDoFooter
  else if TDataSetPDFConfig.GetInstance.PrintDefaultFooter then
    FPDF.CorePDF.OnDoFooter := DefaultFooter;

  if TDataSetPDFConfig.GetInstance.AddPage then
    FPDF.AddPage(TDataSetPDFConfig.GetInstance.Orientation);
end;

destructor TDataSetPDF.Destroy;
begin
  if Assigned(FPDF) then
    FPDF.Free;

  inherited Destroy;
end;

function TDataSetPDF.ToStream: TStream;
var
  LBookMark: TBookmark;
  I: Integer = 0;
  LHasChild: Boolean = False;
begin
  if FDataSet.IsEmpty then
  begin
    if FDownloadFile then
      Result := FPDF.CreateContentStream(csToDownload)
    else
      Result := FPDF.CreateContentStream(csToViewBrowser);
    Exit;
  end;

  LoadFieldStructure;

  FPDF.SetFont(TDataSetPDFConfig.GetInstance.Font,
               TDataSetPDFConfig.GetInstance.FontStyle,
               TDataSetPDFConfig.GetInstance.FontSize,
               False);
  DoTableTitles(FDataSet);

  LBookMark := FDataSet.BookMark;
  try
    FDataSet.First;

    while not FDataSet.Eof do
    begin
      if LHasChild then
        FPDF.LnBreak(5);
      ExportToPDF(FDataSet, LHasChild, FChildRecord, LHasChild);
      FDataSet.Next;
      Inc(I);
    end;

    if FDownloadFile then
      Result := FPDF.CreateContentStream(csToDownload)
    else
      Result := FPDF.CreateContentStream(csToViewBrowser);
  finally
    if FDataSet.BookmarkValid(LBookMark) then
      FDataSet.GotoBookmark(LBookMark);
    FDataSet.FreeBookmark(LBookMark);
  end;
end;

end.

