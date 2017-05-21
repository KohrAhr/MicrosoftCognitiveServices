unit uFunctions.FaceApiAsyncHelper;

interface

uses

  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TBytesStream }
  System.Classes,
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  TFaceApiAsyncCallback = procedure(const AResult: String) of object;

  FaceApiAsyncHelper = class
    /// <summary>
    ///   Implements asynchronous top layout for <see cref="uIFaceApi.Face|IFaceApiFace.DetectURL">interface DetectURL</see>
    /// </summary>
//    class function DetectURL(AAccess: TAccessServer; const AURL: String;
//      const ADetectOptions: TDetectOptions;
//      ACallbackMethod: TFaceApiAsyncCallback): String;
    /// <summary>
    ///   Implements asynchronous top layout for <see cref="uIFaceApi.Face|IFaceApiFace.DetectFile">interface DetectFile</see>
    /// </summary>
    class function DetectFile(AAccess: TAccessServer; const AFileName: String;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback): String;
    /// <summary>
    ///   Implements asynchronous top layout for <see cref="uIFaceApi.Face|IFaceApiFace.DetectStream">interface DetectStream</see>
    /// </summary>
//    class function DetectStream(AAccess: TAccessServer; AStream: TBytesStream;
//      const ADetectOptions: TDetectOptions;
//      ACallbackMethod: TFaceApiAsyncCallback): String;
  end;

implementation

uses
  { TTask.Create }
  System.SysUtils,
  { IFaceApiCore }
  uIFaceApi.Core,
  { TFaceApiCore }
  uFaceApi.Core,
  { IFaceApiFace }
  uIFaceApi.Face,
  { TFaceApiCoreFace }
//  uFaceApi.Core.Face,
  { IFaceApiPersonGroup }
//  uIFaceApi.PersonGroup,
  { ITask }
  System.Threading,
  { TFaceApiCorePersonGroup }
//  uFaceApi.Core.PersonGroup,
  { FaceApiHelper }
  uFunctions.FaceApiHelper;

class function FaceApiAsyncHelper.DetectFile(AAccess: TAccessServer;
  const AFileName: String; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback): String;
var
  LResult: String;
  LTask: ITask;
begin
  LTask := TTask.Create(
    procedure
    begin
      LResult := FaceApiHelper.DetectFile(AAccess, AFileName, ADetectOptions);

      if Assigned(ACallbackMethod) then
        ACallbackMethod(LResult);
    end
  );
  LTask.Start;
end;

//class function FaceApiAsyncHelper.DetectStream(AAccess: TAccessServer;
//  AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;
//begin
//
//end;
//
//class function FaceApiAsyncHelper.DetectURL(AAccess: TAccessServer;
//  const AURL: String; const ADetectOptions: TDetectOptions;
//  ACallbackMethod: TFaceApiAsyncCallback): String;
//begin
//
//end;

end.
