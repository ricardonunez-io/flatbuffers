// automatically generated by the FlatBuffers compiler, do not modify

package NamespaceA.NamespaceB;

import java.nio.*;
import java.lang.*;
import java.util.*;
import com.google.flatbuffers.*;

@SuppressWarnings("unused")
public final class TableInNestedNS extends Table {
  public static void ValidateVersion() { Constants.FLATBUFFERS_24_12_23(); }
  public static TableInNestedNS getRootAsTableInNestedNS(ByteBuffer _bb) { return getRootAsTableInNestedNS(_bb, new TableInNestedNS()); }
  public static TableInNestedNS getRootAsTableInNestedNS(ByteBuffer _bb, TableInNestedNS obj) { _bb.order(ByteOrder.LITTLE_ENDIAN); return (obj.__assign(_bb.getInt(_bb.position()) + _bb.position(), _bb)); }
  public void __init(int _i, ByteBuffer _bb) { __reset(_i, _bb); }
  public TableInNestedNS __assign(int _i, ByteBuffer _bb) { __init(_i, _bb); return this; }

  public int foo() { int o = __offset(4); return o != 0 ? bb.getInt(o + bb_pos) : 0; }
  public boolean mutateFoo(int foo) { int o = __offset(4); if (o != 0) { bb.putInt(o + bb_pos, foo); return true; } else { return false; } }

  public static int createTableInNestedNS(FlatBufferBuilder builder,
      int foo) {
    builder.startTable(1);
    TableInNestedNS.addFoo(builder, foo);
    return TableInNestedNS.endTableInNestedNS(builder);
  }

  public static void startTableInNestedNS(FlatBufferBuilder builder) { builder.startTable(1); }
  public static void addFoo(FlatBufferBuilder builder, int foo) { builder.addInt(0, foo, 0); }
  public static int endTableInNestedNS(FlatBufferBuilder builder) {
    int o = builder.endTable();
    return o;
  }

  public static final class Vector extends BaseVector {
    public Vector __assign(int _vector, int _element_size, ByteBuffer _bb) { __reset(_vector, _element_size, _bb); return this; }

    public TableInNestedNS get(int j) { return get(new TableInNestedNS(), j); }
    public TableInNestedNS get(TableInNestedNS obj, int j) {  return obj.__assign(__indirect(__element(j), bb), bb); }
  }
  public TableInNestedNST unpack() {
    TableInNestedNST _o = new TableInNestedNST();
    unpackTo(_o);
    return _o;
  }
  public void unpackTo(TableInNestedNST _o) {
    int _oFoo = foo();
    _o.setFoo(_oFoo);
  }
  public static int pack(FlatBufferBuilder builder, TableInNestedNST _o) {
    if (_o == null) return 0;
    return createTableInNestedNS(
      builder,
      _o.getFoo());
  }
}

