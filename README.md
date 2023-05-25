# DataSet PDF for Lazarus (FPC)

## ⭕ Prerequisites
- [**dataset-serialize**](https://github.com/viniciussanchez/dataset-serialize).
- [**djpdfreport**](https://github.com/andre-djsystem/djpdfreport).
- [**free-jpdf-pascal**](https://github.com/jepafi/free-jpdf-pascal).

## ⚙️ Installation
Installation is done using the [`boss install`](https://github.com/HashLoad/boss) command:

``` sh
boss install https://github.com/andre-djsystem/dataset-pdf
```

### Manual installation
If you choose to install manually, simply add the following folders to your project, in *Project > Project Options > Paths > Other unit files (-Fu) > Include file search path*
```
../dataset-pdf/src
../djpdfreport
../free-jpdf-pascal
../dataset-serialize/src
```

## ⚡️ Quickstart
All features offered by DataSet PDF are located in the class helper in unit DataSet.PDF. To get your project started, simply add your reference where your functionality is needed. Here's an example:
```pascal
uses DataSet.PDF;
```

## DataSet to PDF
Creating a PDF with information from a DataSet record seems like a very simple task. But that task just got easier. DataSet PDF has two functions for this, namely ToPDFStream and ToPDFFile. Let's look at the use of the functions:

```pascal
begin
  qrySamples.ToPDFStream(''); // Create a TStream
  qrySamples.ToPDFFile('c:\temp\file.pdf'); // Create a PDF File 
end;
``` 
**Parameters**
function ToPDFStream(const AFieldStructure: String; const ADownloadFile: Boolean = True; const AChildRecords: Boolean = True): TStream;
* `AFieldStructure` - File with parameters for each field;
* `ADownloadFile` - Indicates Content-Disposition of PDF(inline/attachment);
* `AChildRecords` -Indicates whether or not to export child records (via master detail or TDataSetField).;
   
procedure ToPDFFile(const AFileName: String; const AFieldStructure: String = ''; const AChildRecords: Boolean = True);   
* `AFileName` - Name of PDF file;
* `AFieldStructure` - File with parameters for each field;
* `AChildRecords` - Indicates whether or not to export child records (via master detail or TDataSetField).;

**Field Structure Example**
``` 
[
	{
		"fieldName": "name",
		"displayWidth": 80,
		"displayLabel": "Name",
		"visible": true
	},    
	{
		"fieldName": "e_mail",
		"displayWidth": 100,
		"displayLabel": "E-mail",
		"visible": true
	},    
	{
		"fieldName": "pass",
		"displayWidth": 60,
		"displayLabel": "Password",
		"visible": true
	},    
	{
		"fieldName": "active",
		"displayWidth": 50,
		"displayLabel": "Active",
		"visible": true
	}    
]
``` 

Inspired in [dataset-serialize](https://github.com/viniciussanchez/dataset-serialize)

## ⚠️ License
`dataset-pdf` is free and open-source library licensed under the [MIT License](https://github.com/andre-djsystem/dataset-pdf/blob/main/LICENSE).
