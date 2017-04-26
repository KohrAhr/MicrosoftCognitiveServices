unit uFunctions.StringHelper;

interface

uses
  { TEncoding }
  System.SysUtils,
	{ TMemoryStream }
	System.Classes;

type
	StringHelper = class
		class function MemoryStreamToString(const M: TMemoryStream): String;
	end;

implementation

class function StringHelper.MemoryStreamToString(const M: TMemoryStream): String;
var
	LStringStream: TStringStream;
begin
	Result := '';

  LStringStream := TStringStream.Create('', TEncoding.UTF8);
  try
    M.Position := 0;
    LStringStream.CopyFrom(M, M.Size);
    Result := LStringStream.DataString;
  finally
    LStringStream.Free;
  end;
end;

end.
