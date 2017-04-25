unit ufmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  uFaceApi, uFaceApi.Servers.Types;

const
  CONST_ACCESS_KEY = '4acb98b9002d4d87878b54bed21af7bc';

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  LFaceApi: TFaceApi;
  LResult: String;
begin
  Memo1.Clear;

  LFaceApi := TFaceApi.Create(CONST_ACCESS_KEY, fasWestUS);
  try
    LResult := LFaceApi.DetectFile('D:\Temp\index.jpg', Detect(True, True, 'age,gender,headPose,smile,facialHair,glasses,emotion'));

    Memo1.Lines.Add(LResult);
  finally
    LFaceApi.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  LFaceApi: TFaceApi;
  LResult: String;
begin
  Memo1.Clear;

  LFaceApi := TFaceApi.Create(CONST_ACCESS_KEY, fasWestUS);
  try
    LResult := LFaceApi.DetectURL('http://1click.lv/index.jpg', Detect(True, True, 'age,gender,headPose,smile,facialHair,glasses,emotion'));

    Memo1.Lines.Add(LResult);
  finally
    LFaceApi.Free;
  end;
end;

end.
