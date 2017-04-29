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
    class function StringToBytesArray(const AValue: String): TArray<System.Byte>;
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

class function StringHelper.StringToBytesArray(const AValue: String): TArray<System.Byte>;
begin
  Result := TEncoding.UTF8.GetBytes(AValue);
end;

end.
