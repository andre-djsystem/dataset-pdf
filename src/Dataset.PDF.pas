unit DataSet.PDF;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Classes, DB, DJPDFReport, libjpfpdf, Generics.Collections, DataSet.Serialize, DataSet.PDF.Export, DataSet.PDF.Config;
{$ENDIF}

type
  TDataSetPDFConfig = DataSet.PDF.Config.TDataSetPDFConfig;

  { TDataSetPDFHelper }

  TDataSetPDFHelper = class helper (TDataSetSerializeHelper) for TDataSet
  public
    function ToPDFStream(const ADownloadFile: Boolean = True; const AChildRecords: Boolean = True): TStream;
    procedure ToPDFFile(const AFileName: String; const AChildRecords: Boolean = True);
  end;

implementation

function TDataSetPDFHelper.ToPDFStream(const ADownloadFile: Boolean;
  const AChildRecords: Boolean): TStream;
var
  LDataSetPDF: TDataSetPDF;
begin
  LDataSetPDF := TDataSetPDF.Create(Self, ADownloadFile, AChildRecords);
  try
    Result := LDataSetPDF.ToStream;
  finally
    LDataSetPDF.Free;
  end;
end;

procedure TDataSetPDFHelper.ToPDFFile(const AFileName: String;
  const AChildRecords: Boolean);
var
  LStream: TMemoryStream;
begin
  if not TDataSet(Self).IsEmpty then
  begin
    LStream := TMemoryStream.Create;
    try
      LStream.LoadFromStream(TDataSet(Self).ToPDFStream(False, AChildRecords));
      LStream.Position := 0;
      LStream.SaveToFile(AFileName);
    finally
      LStream.Free;
    end;
  end;
end;

end.

