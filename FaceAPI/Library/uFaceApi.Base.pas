unit uFaceApi.Base;

interface

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types;

type
  TFaceApiBase = class
  private
    FAccessKey: String;
    FAccessServer: TFaceApiServer;
  public
    property AccessKey: String read FAccessKey write FAccessKey;
    property AccessServer: TFaceApiServer read FAccessServer write FAccessServer;
  end;

implementation

end.
