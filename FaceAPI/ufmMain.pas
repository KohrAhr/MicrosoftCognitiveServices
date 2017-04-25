unit ufmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Net.HttpClient, System.Net.URLClient, System.NetConsts;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  LNameValuePair: TNameValuePair;
  LHTTPClient: THTTPClient;
	LStream: TStream;
  LURL: String;
  LHeaders: TNetHeaders;
	LResponse: TMemoryStream;
begin
  LHTTPClient := THTTPClient.Create;
  try
    SetLength(LHeaders, 1);

    LNameValuePair.Name := 'Ocp-Apim-Subscription-Key';
    LNameValuePair.Value := '4acb98b9002d4d87878b54bed21af7bc';
    LHeaders[0] := LNameValuePair;

    LHTTPClient.ContentType := 'application/octet-stream';
    LURL := 'https://westus.api.cognitive.microsoft.com/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true';
    LStream := LHTTPClient.Post(LURL, 'D:\Temp\index.jpg', nil, LHeaders).ContentStream;

    LResponse := TMemoryStream.Create;
    try
      LResponse.CopyFrom(LStream, LStream.Size);

      LResponse.SaveToFile('D:\temp\answer.json');
    finally
      LResponse.Free;
    end;
  finally
    LHTTPClient.Free;
  end;
end;

end.
