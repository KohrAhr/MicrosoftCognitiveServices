/// <summary>
///   Unit contain the Main Interface declaration for Face API Microsoft
///   Cognitive Services 1.0
///
///   Later part of functions will be moved into
///     "uFaceApi.Person.pas" or into "uFaceApi.FaceList.pas"
/// </summary>
unit uIFaceApi.Core;

interface

uses
  { IFaceApiBase }
  uIFaceApi.Base,
  { TFaceAttributes }
  uFaceApi.FaceAttributes,
  { TBytesStream }
  System.Classes;

type
  /// <summary>
  ///   Main Interface for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  IFaceApiCore = interface(IFaceApiBase)
    ['{904FC5EC-6ECC-49EB-A3B0-7D785A5D23D2}']

    /// <summary>
    ///   Person - Add a Person Face
    ///   Add a representative face to a person for identification. The input
    ///   face is specified as an image with a targetFace rectangle. It returns
    ///   a persistedFaceId representing the added face and this persistedFaceId
    ///   will not expire. Note persistedFaceId is different from faceId which
    ///   represents the detected face by Face - Detect.
    ///
    ///   - The persistedFaceId of person is used in Person - Delete a Person
    ///   Face to remove a face from a person.
    ///   - Each person has a maximum of 248 faces.
    ///   - JPEG, PNG, GIF(the first frame), and BMP are supported. The image
    ///   file size should be larger than or equal to 1KB but no larger than 4MB
    ///   The detectable face size is between 36x36 to 4096x4096 pixels. The
    ///   faces out of this range will not be detected.
    ///   - Rectangle specified by targetFace should contain exactly one face.
    ///   Zero or multiple faces will be regarded as an error. Out of detectable
    ///   face size, large head-pose, or very large occlusions will also result
    ///   in fail to add a person face.
    ///   - The given rectangle specifies both face location and face size at
    ///   the same time. There is no guarantee of correct result if you are
    ///   using rectangle which is not returned from Face - Detect.
    /// </summary>
    /// <param name="AGroupID">
    ///   Specifying the person group containing the target person.
    /// </param>
		/// <param name="APersonID">
    ///   Target person that the face is added to.
		/// </param>
		/// <param name="AURL">
    ///   Internet URL of image. You cannot use intranet URL because it will be
    ///   not accessable by Microsoft Cognitive Services servers.
		/// </param>
		/// <param name="ATargetFace">
    ///   (optional)
    ///   A face rectangle to specify the target face to be added to a person,
    ///   in the format of "targetFace=left,top,width,height". E.g. "targetFace=
    ///   10,10,100,100". If there is more than one face in the image,
    ///   targetFace is required to specify which face to add. No targetFace
    ///   means there is only one face detected in the entire image.
		/// </param>
		/// <param name="AUserData">
    ///   (optional)
    ///   User-specified data about the target face to add for any purpose.
    ///   The maximum length is 1KB.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns the new persistedFaceId.
    ///   JSON fields in response body:
    ///   Fields	        Type	  Description
    ///   persistedFaceId	String	persistedFaceId of the added face, which is
    ///   persisted and will not expire. Different from faceId which is created
    ///   in Face - Detect and will expire in 24 hours after the detection call.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 404, 408, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function AddPersonFaceURL(const AGroupID, APersonID, AURL: String;
      ATargetFace: String; const AUserData: String = ''): String;

    /// <summary>
    ///   Person - Add a Person Face
    ///   Add a representative face to a person for identification. The input
    ///   face is specified as an image with a targetFace rectangle. It returns
    ///   a persistedFaceId representing the added face and this persistedFaceId
    ///   will not expire. Note persistedFaceId is different from faceId which
    ///   represents the detected face by Face - Detect.
    ///
    ///   - The persistedFaceId of person is used in Person - Delete a Person
    ///   Face to remove a face from a person.
    ///   - Each person has a maximum of 248 faces.
    ///   - JPEG, PNG, GIF(the first frame), and BMP are supported. The image
    ///   file size should be larger than or equal to 1KB but no larger than 4MB
    ///   The detectable face size is between 36x36 to 4096x4096 pixels. The
    ///   faces out of this range will not be detected.
    ///   - Rectangle specified by targetFace should contain exactly one face.
    ///   Zero or multiple faces will be regarded as an error. Out of detectable
    ///   face size, large head-pose, or very large occlusions will also result
    ///   in fail to add a person face.
    ///   - The given rectangle specifies both face location and face size at
    ///   the same time. There is no guarantee of correct result if you are
    ///   using rectangle which is not returned from Face - Detect.
    /// </summary>
    /// <param name="AGroupID">
    ///   Specifying the person group containing the target person.
    /// </param>
    /// <param name="APersonID">
    ///   Target person that the face is added to.
		/// </param>
		/// <param name="AStream">
    ///   Stream with image
		/// </param>
		/// <param name="ATargetFace">
    ///   (optional)
    ///   A face rectangle to specify the target face to be added to a person,
    ///   in the format of "targetFace=left,top,width,height". E.g. "targetFace=
    ///   10,10,100,100". If there is more than one face in the image,
    ///   targetFace is required to specify which face to add. No targetFace
    ///   means there is only one face detected in the entire image.
		/// </param>
		/// <param name="AUserData">
    ///   (optional)
    ///   User-specified data about the target face to add for any purpose.
    ///   The maximum length is 1KB.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns the new persistedFaceId.
    ///   JSON fields in response body:
    ///   Fields	        Type	  Description
    ///   persistedFaceId	String	persistedFaceId of the added face, which is
    ///   persisted and will not expire. Different from faceId which is created
    ///   in Face - Detect and will expire in 24 hours after the detection call.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 404, 408, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function AddPersonFaceStream(const AGroupID, APersonID: String; AStream:
      TBytesStream; ATargetFace: String; const AUserData: String = ''): String;
  end;

implementation

end.
