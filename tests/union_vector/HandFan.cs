// <auto-generated>
//  automatically generated by the FlatBuffers compiler, do not modify
// </auto-generated>

using global::System;
using global::System.Collections.Generic;
using global::Google.FlatBuffers;

public struct HandFan : IFlatbufferObject
{
  private Table __p;
  public ByteBuffer ByteBuffer { get { return __p.bb; } }
  public static void ValidateVersion() { FlatBufferConstants.FLATBUFFERS_24_12_23(); }
  public static HandFan GetRootAsHandFan(ByteBuffer _bb) { return GetRootAsHandFan(_bb, new HandFan()); }
  public static HandFan GetRootAsHandFan(ByteBuffer _bb, HandFan obj) { return (obj.__assign(_bb.GetInt(_bb.Position) + _bb.Position, _bb)); }
  public void __init(int _i, ByteBuffer _bb) { __p = new Table(_i, _bb); }
  public HandFan __assign(int _i, ByteBuffer _bb) { __init(_i, _bb); return this; }

  public int Length { get { int o = __p.__offset(4); return o != 0 ? __p.bb.GetInt(o + __p.bb_pos) : (int)0; } }
  public bool MutateLength(int length) { int o = __p.__offset(4); if (o != 0) { __p.bb.PutInt(o + __p.bb_pos, length); return true; } else { return false; } }

  public static Offset<HandFan> CreateHandFan(FlatBufferBuilder builder,
      int length = 0) {
    builder.StartTable(1);
    HandFan.AddLength(builder, length);
    return HandFan.EndHandFan(builder);
  }

  public static void StartHandFan(FlatBufferBuilder builder) { builder.StartTable(1); }
  public static void AddLength(FlatBufferBuilder builder, int length) { builder.AddInt(0, length, 0); }
  public static Offset<HandFan> EndHandFan(FlatBufferBuilder builder) {
    int o = builder.EndTable();
    return new Offset<HandFan>(o);
  }
  public HandFanT UnPack() {
    var _o = new HandFanT();
    this.UnPackTo(_o);
    return _o;
  }
  public void UnPackTo(HandFanT _o) {
    _o.Length = this.Length;
  }
  public static Offset<HandFan> Pack(FlatBufferBuilder builder, HandFanT _o) {
    if (_o == null) return default(Offset<HandFan>);
    return CreateHandFan(
      builder,
      _o.Length);
  }
}

public class HandFanT
{
  [Newtonsoft.Json.JsonProperty("length")]
  public int Length { get; set; }

  public HandFanT() {
    this.Length = 0;
  }
}


static public class HandFanVerify
{
  static public bool Verify(Google.FlatBuffers.Verifier verifier, uint tablePos)
  {
    return verifier.VerifyTableStart(tablePos)
      && verifier.VerifyField(tablePos, 4 /*Length*/, 4 /*int*/, 4, false)
      && verifier.VerifyTableEnd(tablePos);
  }
}
