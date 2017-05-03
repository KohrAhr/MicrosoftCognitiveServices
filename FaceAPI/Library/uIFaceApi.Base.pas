/// <summary>
///   Unit contain the Main Base Interface declaration for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uIFaceApi.Base;

interface

uses
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  /// <summary>
  ///   Main Interface for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  IFaceApiBase = interface(IInterface)
    ['{F67A0A71-4171-4145-99FF-65343ED786B0}']

    /// <summary>
    ///   Define your access key and define your server location
    /// </summary>
		/// <param name="AAccess">
    ///   - Subscription key which provides access to this API. Found in your Cognitive Services accounts.
    ///   - Cognitive Api server location
		/// </param>
    procedure SetAccessKey(const AAccess: TAccessServer);
end;

implementation

end.
