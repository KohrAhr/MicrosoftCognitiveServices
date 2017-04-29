unit uIFaceApi;

interface

uses
  { TStringStream }
  System.Classes,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions;

type
  IFaceApi = interface
    ['{904FC5EC-6ECC-49EB-A3B0-7D785A5D23D2}']

    function DetectURL(AURL: String; ADetectOptions: TDetectOptions): String;
    function DetectFile(AFileName: String; ADetectOptions: TDetectOptions): String;
    function DetectStream(AStream: TBytesStream; ADetectOptions: TDetectOptions): String;

    function ListPersonGroups(AStart: String = ''; ATop: Integer = 1000): String;

    function ListPersonsInPersonGroup(APersonGroup: String): String;
  end;

implementation

end.
