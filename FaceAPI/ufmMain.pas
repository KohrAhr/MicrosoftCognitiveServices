unit ufmMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    btnDetectInFile: TButton;
    memLog: TMemo;
    btnDetectInUrl: TButton;
    btnDetectInStream: TButton;
    procedure btnDetectInFileClick(Sender: TObject);
    procedure btnDetectInUrlClick(Sender: TObject);
    procedure btnDetectInStreamClick(Sender: TObject);
  private
  public
  end;

var
  fmMain: TfmMain;

implementation

uses
  { TFaceApi }
  uFaceApi,
  uFaceApi.Servers.Types;

const
  CONST_ACCESS_KEY = '4acb98b9002d4d87878b54bed21af7bc';

{$R *.dfm}

procedure TfmMain.btnDetectInFileClick(Sender: TObject);
var
  LFaceApi: TFaceApi;
  LResult: String;
begin
  memLog.Clear;

  LFaceApi := TFaceApi.Create(CONST_ACCESS_KEY, fasWestUS);
  try
    LResult := LFaceApi.DetectFile('C:\Temp\index.jpg', Detect(True, True, [doAge, doGender, doHeadPost, doSmile, doFacialHair, doGlasses, doEmotion]));

    memLog.Lines.Add(LResult);
  finally
    LFaceApi.Free;
  end;
end;

procedure TfmMain.btnDetectInUrlClick(Sender: TObject);
var
  LFaceApi: TFaceApi;
  LResult: String;
begin
  memLog.Clear;

  LFaceApi := TFaceApi.Create(CONST_ACCESS_KEY, fasWestUS);
  try
    LResult := LFaceApi.DetectURL('http://1click.lv/index.jpg', Detect(True));

    memLog.Lines.Add(LResult);
  finally
    LFaceApi.Free;
  end;
end;

procedure TfmMain.btnDetectInStreamClick(Sender: TObject);
var
  LFaceApi: TFaceApi;
  LResult: String;
  LRequestContent: TStringStream;
begin
  LRequestContent := nil;

  memLog.Clear;

  LFaceApi := TFaceApi.Create(CONST_ACCESS_KEY, fasWestUS);
  try
    LRequestContent := TStringStream.Create;

    LRequestContent.LoadFromFile('C:\Temp\index.jpg');

    LResult := LFaceApi.DetectStream(LRequestContent, Detect(True, True));

    memLog.Lines.Add(LResult);
  finally
    LFaceApi.Free;
  end;
end;

end.
