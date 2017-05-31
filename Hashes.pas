unit Hashes;

interface

uses Windows, SysUtils, Classes;

type
 PMD5Digest = ^TMD5Digest;
 TMD5Digest = record
  case Integer of
   0: (A, B, C, D: LongInt);
   1: (v: array [0..15] of Byte);
 end;

function MD5String_(const S: string): TMD5Digest;
function MD5String(const S: string): string;
function MD5File_(const FileName: string): TMD5Digest;
function MD5File(const FileName: string): string;
function MD5Stream_(const Stream: TStream): TMD5Digest;
function MD5Stream(const Stream: TStream): string;
function MD5Buffer_(const Buffer; Size: Integer): TMD5Digest;
function MD5Buffer(const Buffer; Size: Integer): string;

function CRC32String_(str: string): Longword;
function CRC32File_(FileName: string): Longword;
function CRC32Stream_(Stream: TStream): Longword;
function CRC32String(str: string): string;
function CRC32File(FileName: string): string;
function CRC32Stream(Stream: TStream): string;

const
 BufferSize: integer = 65536;

implementation

type
 UINT4 = LongWord;

 PArray4UINT4 = ^TArray4UINT4;
 TArray4UINT4 = array [0..3] of UINT4;
 PArray2UINT4 = ^TArray2UINT4;
 TArray2UINT4 = array [0..1] of UINT4;
 PArray16Byte = ^TArray16Byte;
 TArray16Byte = array [0..15] of Byte;
 PArray64Byte = ^TArray64Byte;
 TArray64Byte = array [0..63] of Byte;

 PByteArray = ^TByteArray;
 TByteArray = array [0..0] of Byte;

 PUINT4Array = ^TUINT4Array;
 TUINT4Array = array [0..0] of UINT4;

 PMD5Context = ^TMD5Context;
 TMD5Context = record
   state: TArray4UINT4;
   count: TArray2UINT4;
   buffer: TArray64Byte;
 end;

 TBuffer = array[0..1048576] of byte;
 TCrc32Table = array[0..255] of LongWord;//integer / MirekP 2004-01-27

 const
 Crc32Table: TCrc32Table =
 ($0,$77073096,$EE0E612C,$990951BA,
 $76DC419,$706AF48F,$E963A535,$9E6495A3,
 $EDB8832,$79DCB8A4,$E0D5E91E,$97D2D988,
 $9B64C2B,$7EB17CBD,$E7B82D07,$90BF1D91,
 $1DB71064,$6AB020F2,$F3B97148,$84BE41DE,
 $1ADAD47D,$6DDDE4EB,$F4D4B551,$83D385C7,
 $136C9856,$646BA8C0,$FD62F97A,$8A65C9EC,
 $14015C4F,$63066CD9,$FA0F3D63,$8D080DF5,
 $3B6E20C8,$4C69105E,$D56041E4,$A2677172,
 $3C03E4D1,$4B04D447,$D20D85FD,$A50AB56B,
 $35B5A8FA,$42B2986C,$DBBBC9D6,$ACBCF940,
 $32D86CE3,$45DF5C75,$DCD60DCF,$ABD13D59,
 $26D930AC,$51DE003A,$C8D75180,$BFD06116,
 $21B4F4B5,$56B3C423,$CFBA9599,$B8BDA50F,
 $2802B89E,$5F058808,$C60CD9B2,$B10BE924,
 $2F6F7C87,$58684C11,$C1611DAB,$B6662D3D,
 $76DC4190,$1DB7106,$98D220BC,$EFD5102A,
 $71B18589,$6B6B51F,$9FBFE4A5,$E8B8D433,
 $7807C9A2,$F00F934,$9609A88E,$E10E9818,
 $7F6A0DBB,$86D3D2D,$91646C97,$E6635C01,
 $6B6B51F4,$1C6C6162,$856530D8,$F262004E,
 $6C0695ED,$1B01A57B,$8208F4C1,$F50FC457,
 $65B0D9C6,$12B7E950,$8BBEB8EA,$FCB9887C,
 $62DD1DDF,$15DA2D49,$8CD37CF3,$FBD44C65,
 $4DB26158,$3AB551CE,$A3BC0074,$D4BB30E2,
 $4ADFA541,$3DD895D7,$A4D1C46D,$D3D6F4FB,
 $4369E96A,$346ED9FC,$AD678846,$DA60B8D0,
 $44042D73,$33031DE5,$AA0A4C5F,$DD0D7CC9,
 $5005713C,$270241AA,$BE0B1010,$C90C2086,
 $5768B525,$206F85B3,$B966D409,$CE61E49F,
 $5EDEF90E,$29D9C998,$B0D09822,$C7D7A8B4,
 $59B33D17,$2EB40D81,$B7BD5C3B,$C0BA6CAD,
 $EDB88320,$9ABFB3B6,$3B6E20C,$74B1D29A,
 $EAD54739,$9DD277AF,$4DB2615,$73DC1683,
 $E3630B12,$94643B84,$D6D6A3E,$7A6A5AA8,
 $E40ECF0B,$9309FF9D,$A00AE27,$7D079EB1,
 $F00F9344,$8708A3D2,$1E01F268,$6906C2FE,
 $F762575D,$806567CB,$196C3671,$6E6B06E7,
 $FED41B76,$89D32BE0,$10DA7A5A,$67DD4ACC,
 $F9B9DF6F,$8EBEEFF9,$17B7BE43,$60B08ED5,
 $D6D6A3E8,$A1D1937E,$38D8C2C4,$4FDFF252,
 $D1BB67F1,$A6BC5767,$3FB506DD,$48B2364B,
 $D80D2BDA,$AF0A1B4C,$36034AF6,$41047A60,
 $DF60EFC3,$A867DF55,$316E8EEF,$4669BE79,
 $CB61B38C,$BC66831A,$256FD2A0,$5268E236,
 $CC0C7795,$BB0B4703,$220216B9,$5505262F,
 $C5BA3BBE,$B2BD0B28,$2BB45A92,$5CB36A04,
 $C2D7FFA7,$B5D0CF31,$2CD99E8B,$5BDEAE1D,
 $9B64C2B0,$EC63F226,$756AA39C,$26D930A,
 $9C0906A9,$EB0E363F,$72076785,$5005713,
 $95BF4A82,$E2B87A14,$7BB12BAE,$CB61B38,
 $92D28E9B,$E5D5BE0D,$7CDCEFB7,$BDBDF21,
 $86D3D2D4,$F1D4E242,$68DDB3F8,$1FDA836E,
 $81BE16CD,$F6B9265B,$6FB077E1,$18B74777,
 $88085AE6,$FF0F6A70,$66063BCA,$11010B5C,
 $8F659EFF,$F862AE69,$616BFFD3,$166CCF45,
 $A00AE278,$D70DD2EE,$4E048354,$3903B3C2,
 $A7672661,$D06016F7,$4969474D,$3E6E77DB,
 $AED16A4A,$D9D65ADC,$40DF0B66,$37D83BF0,
 $A9BCAE53,$DEBB9EC5,$47B2CF7F,$30B5FFE9,
 $BDBDF21C,$CABAC28A,$53B39330,$24B4A3A6,
 $BAD03605,$CDD70693,$54DE5729,$23D967BF,
 $B3667A2E,$C4614AB8,$5D681B02,$2A6F2B94,
 $B40BBE37,$C30C8EA1,$5A05DF1B,$2D02EF8D);


const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;

var
 Padding : TArray64Byte =
 ($80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);


function _F(x, y, z: UINT4): UINT4;
begin
 Result := (((x) and (y)) or ((not x) and (z)));
end;

function _G(x, y, z: UINT4): UINT4;
begin
 Result := (((x) and (z)) or ((y) and (not z)));
end;

function _H(x, y, z: UINT4): UINT4;
begin
 Result := ((x) xor (y) xor (z));
end;

function _I(x, y, z: UINT4): UINT4;
begin
 Result := ((y) xor ((x) or ( not z)));
end;

function ROTATE_LEFT(x, n: UINT4): UINT4;
begin
 Result := (((x) shl (n)) or ((x) shr (32-(n))));
end;

procedure FF(var a: UINT4; b, c, d, x, s, ac: UINT4);
begin
  a := a + _F(b, c, d) + x + ac;
  a := ROTATE_LEFT (a, s);
  a := a + b;
end;

procedure GG(var a: UINT4; b, c, d, x, s, ac: UINT4);
begin
 a := a + _G(b, c, d) + x + ac;
 a := ROTATE_LEFT(a, s);
 a := a + b;
end;

procedure HH(var a: UINT4; b, c, d, x, s, ac: UINT4);
begin
 a := a + _H(b, c, d) + x + ac;
 a := ROTATE_LEFT(a, s);
 a := a + b;
end;

procedure II(var a: UINT4; b, c, d, x, s, ac: UINT4);
begin
 a := a + _I(b, c, d) + x + ac;
 a := ROTATE_LEFT(a, s);
 a := a + b;
end;

procedure MD5Encode(Output: PByteArray; Input: PUINT4Array; Len: LongWord);
var
 i, j: LongWord;
begin
 j:=0;
 i:=0;
 while j < Len do  begin
  output[j] := Byte(input[i] and $ff);
  output[j+1] := Byte((input[i] shr 8) and $ff);
  output[j+2] := Byte((input[i] shr 16) and $ff);
  output[j+3] := Byte((input[i] shr 24) and $ff);
  Inc(j, 4);
  Inc(i);
 end;
end;

procedure MD5Decode(Output: PUINT4Array; Input: PByteArray; Len: LongWord);
var
 i, j: LongWord;
begin
 j:=0;
 i:=0;
 while j < Len do begin
  Output[i] := UINT4(input[j]) or (UINT4(input[j+1]) shl 8) or
   (UINT4(input[j+2]) shl 16) or ( UINT4(input[j+3]) shl 24);
  Inc(j, 4);
  Inc(i);
 end;
end;

procedure MD5_memcpy(Output: PByteArray; Input: PByteArray; Len: LongWord);
begin
 Move(Input^, Output^, Len);
end;

procedure MD5_memset(Output: PByteArray; Value: Integer; Len: LongWord);
begin
 FillChar(Output^, Len, Byte(Value));
end;

procedure MD5Transform(State: PArray4UINT4; Buffer: PArray64Byte);
var
 a, b, c, d: UINT4;
 x : array[0..15] of UINT4;
begin
 a:=State[0]; b:=State[1]; c:=State[2]; d:=State[3];
 MD5Decode(PUINT4Array(@x), PByteArray(Buffer), 64);

 FF (a, b, c, d, x[ 0], S11, $d76aa478);
 FF (d, a, b, c, x[ 1], S12, $e8c7b756);
 FF (c, d, a, b, x[ 2], S13, $242070db);
 FF (b, c, d, a, x[ 3], S14, $c1bdceee);
 FF (a, b, c, d, x[ 4], S11, $f57c0faf);
 FF (d, a, b, c, x[ 5], S12, $4787c62a);
 FF (c, d, a, b, x[ 6], S13, $a8304613);
 FF (b, c, d, a, x[ 7], S14, $fd469501);
 FF (a, b, c, d, x[ 8], S11, $698098d8);
 FF (d, a, b, c, x[ 9], S12, $8b44f7af);
 FF (c, d, a, b, x[10], S13, $ffff5bb1);
 FF (b, c, d, a, x[11], S14, $895cd7be);
 FF (a, b, c, d, x[12], S11, $6b901122);
 FF (d, a, b, c, x[13], S12, $fd987193);
 FF (c, d, a, b, x[14], S13, $a679438e);
 FF (b, c, d, a, x[15], S14, $49b40821);

 GG (a, b, c, d, x[ 1], S21, $f61e2562);
 GG (d, a, b, c, x[ 6], S22, $c040b340);
 GG (c, d, a, b, x[11], S23, $265e5a51);
 GG (b, c, d, a, x[ 0], S24, $e9b6c7aa);
 GG (a, b, c, d, x[ 5], S21, $d62f105d);
 GG (d, a, b, c, x[10], S22,  $2441453);
 GG (c, d, a, b, x[15], S23, $d8a1e681);
 GG (b, c, d, a, x[ 4], S24, $e7d3fbc8);
 GG (a, b, c, d, x[ 9], S21, $21e1cde6);
 GG (d, a, b, c, x[14], S22, $c33707d6);
 GG (c, d, a, b, x[ 3], S23, $f4d50d87);

 GG (b, c, d, a, x[ 8], S24, $455a14ed);
 GG (a, b, c, d, x[13], S21, $a9e3e905);
 GG (d, a, b, c, x[ 2], S22, $fcefa3f8);
 GG (c, d, a, b, x[ 7], S23, $676f02d9);
 GG (b, c, d, a, x[12], S24, $8d2a4c8a);

 HH (a, b, c, d, x[ 5], S31, $fffa3942);
 HH (d, a, b, c, x[ 8], S32, $8771f681);
 HH (c, d, a, b, x[11], S33, $6d9d6122);
 HH (b, c, d, a, x[14], S34, $fde5380c);
 HH (a, b, c, d, x[ 1], S31, $a4beea44);
 HH (d, a, b, c, x[ 4], S32, $4bdecfa9);
 HH (c, d, a, b, x[ 7], S33, $f6bb4b60);
 HH (b, c, d, a, x[10], S34, $bebfbc70);
 HH (a, b, c, d, x[13], S31, $289b7ec6);
 HH (d, a, b, c, x[ 0], S32, $eaa127fa);
 HH (c, d, a, b, x[ 3], S33, $d4ef3085);
 HH (b, c, d, a, x[ 6], S34,  $4881d05);
 HH (a, b, c, d, x[ 9], S31, $d9d4d039);
 HH (d, a, b, c, x[12], S32, $e6db99e5);
 HH (c, d, a, b, x[15], S33, $1fa27cf8);
 HH (b, c, d, a, x[ 2], S34, $c4ac5665);

 II (a, b, c, d, x[ 0], S41, $f4292244);
 II (d, a, b, c, x[ 7], S42, $432aff97);
 II (c, d, a, b, x[14], S43, $ab9423a7);
 II (b, c, d, a, x[ 5], S44, $fc93a039);
 II (a, b, c, d, x[12], S41, $655b59c3);
 II (d, a, b, c, x[ 3], S42, $8f0ccc92);
 II (c, d, a, b, x[10], S43, $ffeff47d);
 II (b, c, d, a, x[ 1], S44, $85845dd1);
 II (a, b, c, d, x[ 8], S41, $6fa87e4f);
 II (d, a, b, c, x[15], S42, $fe2ce6e0);
 II (c, d, a, b, x[ 6], S43, $a3014314);
 II (b, c, d, a, x[13], S44, $4e0811a1);
 II (a, b, c, d, x[ 4], S41, $f7537e82);
 II (d, a, b, c, x[11], S42, $bd3af235);
 II (c, d, a, b, x[ 2], S43, $2ad7d2bb);
 II (b, c, d, a, x[ 9], S44, $eb86d391);

 Inc(State[0], a);
 Inc(State[1], b);
 Inc(State[2], c);
 Inc(State[3], d);

 MD5_memset (PByteArray(@x), 0, SizeOf (x));
end;


procedure MD5Init(var Context: TMD5Context);
begin
 FillChar(Context, SizeOf(Context), 0);
 Context.state[0] := $67452301;
 Context.state[1] := $efcdab89;
 Context.state[2] := $98badcfe;
 Context.state[3] := $10325476;
end;

procedure MD5Update(var Context: TMD5Context; Input: PByteArray; InputLen: LongWord);
var
 i, index, partLen: LongWord;

begin
 index := LongWord( (context.count[0] shr 3) and $3F);
 Inc(Context.count[0], UINT4(InputLen) shl 3);
 if Context.count[0] < UINT4(InputLen) shl 3 then Inc(Context.count[1]);
 Inc(Context.count[1], UINT4(InputLen) shr 29);
 partLen := 64 - index;
 if inputLen >= partLen then begin
  MD5_memcpy(PByteArray(@Context.buffer[index]), Input, PartLen);
  MD5Transform(@Context.state, @Context.buffer);
  i := partLen;
  while i + 63 < inputLen do begin
   MD5Transform(@Context.state, PArray64Byte(@Input[i]));
   Inc(i, 64);
  end;
  index := 0;
 end else i:=0;
 MD5_memcpy(PByteArray(@Context.buffer[index]), PByteArray(@Input[i]), inputLen - i);
end;


procedure MD5Final(var Digest: TMD5Digest; var Context: TMD5Context);
var
 bits: array [0..7] of Byte;
 index, padLen: LongWord;
begin
 MD5Encode(PByteArray(@bits), PUINT4Array(@Context.count), 8);
 index := LongWord( (Context.count[0] shr 3) and $3F);
 if index < 56 then padLen := 56 - index else padLen := 120 - index;
 MD5Update(Context, PByteArray(@PADDING), padLen);
 MD5Update(Context, PByteArray(@Bits), 8);
 MD5Encode(PByteArray(@Digest), PUINT4Array(@Context.state), 16);
 MD5_memset(PByteArray(@Context), 0, SizeOf(Context));
end;

function MD5DigestToStr(const Digest: TMD5Digest): string;
var
 i: Integer;
begin
 Result:='';
 for i:=0 to 15 do Result:=Result+IntToHex(Digest.v[i], 2);
end;

function MD5String(const S: string): string;
begin
  Result := MD5DigestToStr(MD5String_(s));
end;

function MD5String_(const S: string): TMD5Digest;
begin
  Result := MD5Buffer_(PChar(S)^, Length(S));
end;

function MD5File_(const FileName: string): TMD5Digest;
var
 F: TFileStream;
begin
 F:=TFileStream.Create(FileName, fmOpenRead);
 try
  Result:=MD5Stream_(F);
 finally
  F.Free;
 end;
end;

function MD5File(const FileName: string): string;
begin
  Result := MD5DigestToStr(MD5File_(FileName));
end;


function MD5Stream_(const Stream: TStream): TMD5Digest;
var
 Context: TMD5Context;
 Buffer: array[0..4095] of Byte;
 Size: Integer;
 ReadBytes : Integer;
 TotalBytes : Integer;
 SavePos: Integer;
begin
 MD5Init(Context);
 Size:=Stream.Size;
 SavePos:=Stream.Position;
 TotalBytes:=0;
 try
  Stream.Seek(0, soFromBeginning);
  repeat
   ReadBytes:=Stream.Read(Buffer, SizeOf(Buffer));
   Inc(TotalBytes, ReadBytes);
   MD5Update(Context, @Buffer, ReadBytes);
  until (ReadBytes = 0) or (TotalBytes = Size);
 finally
  Stream.Seek(SavePos, soFromBeginning);
 end;
 MD5Final(Result, Context);
end;

function MD5Stream(const Stream: TStream): string;
begin
  Result := MD5DigestToStr(MD5Stream_(Stream));
end;

function MD5Buffer_(const Buffer; Size: Integer): TMD5Digest;
var
 Context: TMD5Context;
begin
 MD5Init(Context);
 MD5Update(Context, PByteArray(@Buffer), Size);
 MD5Final(Result, Context);
end;

function MD5Buffer(const Buffer; Size: Integer): string;
begin
  Result := MD5DigestToStr(MD5Buffer_(Buffer, Size));
end;

//=================================================================================

function UpdateCrc32(Value: LongWord; var Buffer: array of byte; Count: integer): LongWord;
var
  i: integer;
begin
  Result:=Value;
  for i:=0 to Count-1 do
  begin
    Result:=((Result shr 8) and $00FFFFFF) xor Crc32Table[(Result xor Buffer[i]) and $000000FF];
  end;
end;

function AsmUpdateCrc32(Value: LongWord; Buffer: pointer; Count: integer): LongWord; assembler;
asm
  {Input = eax: Value, edx: Points to Buffer, ecx: Count}
  {Output= eax: CRC32}
  push ebx
  push edi
  push esi
  mov  esi, edx                    {esi: Points to Buffer}
  mov  edx, eax                    {edx: Result}
  mov  edi, Offset Crc32Table      {edi: Points to Crc32Table}
  xor  eax, eax
  cld
  @@Loop:
  mov  ebx, edx                    {Save Result in ebx}
  shr  edx, 8
  and  edx, 00FFFFFFh
  lodsb                            {Load next Buffer entry}
  xor  ebx, eax
  and  ebx, 000000FFh
  xor  edx, dword ptr [edi+4*ebx]
  dec  ecx                         {Dec Count}
  jnz  @@Loop                      {if Count<>0 goto @@Loop}
  mov  eax, edx                    {Save Result in eax}
  pop  esi
  pop  edi
  pop  ebx
end;

function CRC32File_(FileName: string): Longword; //Integer;
var
  Buffer: Pointer;
  Handle,Loaded: integer;
begin
  Result:=$FFFFFFFF;
  GetMem(Buffer,BufferSize);
  Handle:=FileOpen(FileName,fmOpenRead);
  repeat
    Loaded:=FileRead(Handle,Buffer^,BufferSize);
    Result:=UpdateCrc32(Result,TBuffer(Buffer^),Loaded);
  until Loaded<>BufferSize;
  FileClose(Handle);
  FreeMem(Buffer);
  Result:=not Result;
end;


function CRC32Stream_(Stream: TStream): Longword; //Integer;
var
  Value: byte;
begin
  Result:=$FFFFFFFF;
  while Stream.Read(Value,1)=1 do
    Result:=((Result shr 8) and $00FFFFFF) xor Crc32Table[(Result xor Value) and $000000FF];
  Result:=not Result;
end;

function CRC32Stream(Stream: TStream): string;
begin
  Result := IntToHex(CRC32Stream_(Stream),8);
end;

function CRC32String_(str: string): Longword;
var
  ss: TStringStream;
begin
  try
    ss := TStringStream.Create(str);
    Result:= CRC32Stream_(ss);
  finally
    ss.Free;
  end;
end;

function CRC32String(str: string): string;
begin
  Result := IntToHex(CRC32String_(str),8);
end;

function CRC32File(FileName: string): string;
begin
  Result := IntToHex(CRC32File_(FileName),8);
end;

end.
