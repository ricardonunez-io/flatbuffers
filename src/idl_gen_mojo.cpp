/*
 * Copyright 2024 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "idl_gen_mojo.h"

#include <string>
#include <unordered_set>

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"

namespace flatbuffers {
namespace mojo {

class MojoGenerator : public BaseGenerator {
 private:
  CodeWriter code_;
 public:
  MojoGenerator(const Parser &parser, const std::string &path,
                   const std::string &file_name)
      : BaseGenerator(parser, path, file_name, "" /* not used */, ".",
                      "mojo") {
    static const char *const keywords[] = {
      "None",   "True",    "False",     "return",  "struct",    "class",
      "import", "Int",     "float",     "String",  "any",       "def",
      "is",     "from",    "program",   "private", "coroutine", "resource",
      "enum",   "typeof",  "var",       "let",     "pakfile",   "switch",
      "case",   "default", "namespace", "not",     "and",       "or",
      "Bool",   "List",
    };
    keywords_.insert(std::begin(keywords), std::end(keywords));
    code_.SetPadding("    ");
  }

  std::string EscapeKeyword(const std::string &name) const {
    return keywords_.find(name) == keywords_.end() ? name : name + "_";
  }

  std::string NormalizedName(const Definition &definition) const {
    return EscapeKeyword(definition.name);
  }

  std::string NormalizedName(const EnumVal &ev) const {
    return EscapeKeyword(ev.name);
  }

  std::string NamespacedName(const Definition &def) {
    return WrapInNameSpace(def.defined_namespace, NormalizedName(def));
  }

  std::string GenTypeName(const Type &type) {
    auto bits = NumToString(SizeOf(type.base_type) * 8);
    if (IsInteger(type.base_type)) {
      if (IsUnsigned(type.base_type))
        return "uint" + bits;
      else
        return "int" + bits;
    }
    if (IsFloat(type.base_type)) return "float" + bits;
    if (IsString(type)) return "string";
    if (type.base_type == BASE_TYPE_STRUCT) return "table";
    return "none";
  }

  std::string EnumTypeName(const Type &type) {
    auto bits = NumToString(SizeOf(type.base_type) * 8);
    if (IsInteger(type.base_type)) {
      if (IsUnsigned(type.base_type))
        return "UInt" + bits;
      else
        return "Int" + bits;
    }
    return "None";
  }

  void GenComment(const std::vector<std::string> &dc) {
    if (dc.begin() == dc.end()) {
      // Don't output empty comment blocks with 0 lines of comment content.
      return;
    }
    for (auto it = dc.begin(); it != dc.end(); ++it) { code_ += "# " + *it; }
  }

  std::string DTypeName(const Type &type) {
    auto bits = NumToString(SizeOf(type.base_type) * 8);
    if (IsInteger(type.base_type)) {
      if (IsUnsigned(type.base_type))
        return "DType.uint" + bits;
      else
        return "DType.int" + bits;
    }
    if (IsBool(type.base_type)) {
      return "DType.bool";
    }
    return "None";
  }

  std::string MojoType(const Type &type) {
    auto bits = NumToString(SizeOf(type.base_type) * 8);
    if (IsFloat(type.base_type)) return "Float" + bits;
    if (IsBool(type.base_type)) return "Scalar[DType.bool]";
    if (IsScalar(type.base_type) && type.enum_def)
      return NormalizedName(*type.enum_def);
    if (!IsScalar(type.base_type)) return "Int32";
    if (IsString(type)) return "String";
    if (IsUnsigned(type.base_type)) return "UInt" + bits;
    return "Int" + bits;
  }

  std::string MojoTypeCreate(const Type &type) {
    auto bits = NumToString(SizeOf(type.base_type) * 8);
    if (IsFloat(type.base_type)) return "Float" + bits;
    if (IsBool(type.base_type)) return "Scalar[DType.bool]";
    if (IsScalar(type.base_type) && type.enum_def)
      return NormalizedName(*type.enum_def);
    if (!IsScalar(type.base_type)) return "flatbuffers.Offset";
    if (IsString(type)) return "flatbuffers.Offset";
    if (IsUnsigned(type.base_type)) return "UInt" + bits;
    return "Int" + bits;
  }

  std::string MojoArgType(const Type &type) {
    auto bits = NumToString(SizeOf(type.base_type) * 8);
    if (IsFloat(type.base_type)) return "Float" + bits;
    if (IsBool(type.base_type)) return "Scalar[DType.bool]";
    if (IsScalar(type.base_type) && type.enum_def)
      return NormalizedName(*type.enum_def);
    if (IsVectorOfTable(type)) return "List[flatbuffers.Offset]";
    if (IsVector(type) && !IsVectorOfStruct(type)) return "List[" + MojoType(type.VectorType()) + "]";
    if (IsString(type)) return "Optional[StringRef]";
    if (IsStruct(type)) return "Optional[" + NormalizedName(*type.struct_def) + "VO]";
    if (!IsScalar(type.base_type)) return "Optional[flatbuffers.Offset]";
    return "Int" + bits;
  }

  std::string MojoArgDefault(const FieldDef &def) {
    if (IsVectorOfTable(def.value.type)) {
      return "List[flatbuffers.Offset]()";
    }
    if (IsVector(def.value.type) && !IsVectorOfStruct(def.value.type)) {
      return "List[" + MojoType(def.value.type.VectorType()) + "]()";
    }
    if (def.IsOptional()) {
      return "None";
    }
    if (!IsScalar(def.value.type.base_type) || IsString(def.value.type)){
      return "None";
    }
    if (IsEnum(def.value.type)) {
      return  NormalizedName(*def.value.type.enum_def) + "(" + def.value.constant + ")";
    }
    return def.value.constant;
  }

  // Returns the method name for use with add/put calls.
  std::string GenMethod(const Type &type) {
    return IsScalar(type.base_type)
               ? ConvertCase(GenTypeBasic(type), Case::kUpperCamel)
               : (IsStruct(type) ? "Struct" : "UOffsetTRelative");
  }

  // This uses Python names for now..
  std::string GenTypeBasic(const Type &type) {
    // clang-format off
    static const char *ctypename[] = {
      #define FLATBUFFERS_TD(ENUM, IDLTYPE, \
              CTYPE, JTYPE, GTYPE, NTYPE, PTYPE, ...) \
        #PTYPE,
        FLATBUFFERS_GEN_TYPES(FLATBUFFERS_TD)
      #undef FLATBUFFERS_TD
    };
    // clang-format on
    return ctypename[type.base_type];
  }

  // Generate a struct field, conditioned on its child type(s).
  void GenStructAccessor(const StructDef &struct_def, const FieldDef &field) {
    GenComment(field.doc_comment);
    code_.SetValue("NAME", NormalizedName(field));
    code_.SetValue("OFFSET", NumToString(field.value.offset));

    if (IsScalar(field.value.type.base_type)) {
      if (field.IsOptional()) {
        code_.SetValue("TYPE_NAME", "Optional[" + MojoType(field.value.type) + "]");
      } else {
        code_.SetValue("TYPE_NAME", MojoType(field.value.type));
      }
      code_ += "fn {{NAME}}(self) -> {{TYPE_NAME}}:";
      code_.IncrementIdentLevel();
      if (struct_def.fixed) {
        code_.SetValue("DTYPE", DTypeName(field.value.type));
        code_ += "return flatbuffers.read[{{DTYPE}}](self._buf, int(self._pos) + {{OFFSET}})";
      } else {
        auto defval = field.IsOptional()
                          ? (IsFloat(field.value.type.base_type) ? "0.0" : "0")
                          : field.value.constant;
        code_.SetValue("DTYPE", DTypeName(field.value.type));
        code_.SetValue("DEFVAL", defval);
        code_ += "return flatbuffers.field[{{DTYPE}}](self._buf, int(self._pos), {{OFFSET}}, {{DEFVAL}})";
      }
      code_.DecrementIdentLevel();
      code_ += "";
      return;
    }

    switch (field.value.type.base_type) {
      case BASE_TYPE_STRUCT: {

        code_.SetValue("TYPE_NAME", NormalizedName(*field.value.type.struct_def));
        if (struct_def.fixed) {
          code_ += "fn {{NAME}}(self) -> {{TYPE_NAME}}:";
          code_.IncrementIdentLevel();
          code_ += "return {{TYPE_NAME}}(self._buf, int(self._pos), {{OFFSET}})";
          code_.DecrementIdentLevel();
          code_ += "";
        } else {
          if (!field.IsRequired()){
            code_.SetValue("TYPE_NAME", "Optional[" + NormalizedName(*field.value.type.struct_def) + "]");
          }
          code_ += "fn {{NAME}}(self) -> {{TYPE_NAME}}:";
          code_.IncrementIdentLevel();
          code_.SetValue("SOT", field.value.type.struct_def->fixed ? "struct" : "table");
          code_ += "var o = flatbuffers.field_{{SOT}}(self._buf, int(self._pos), {{OFFSET}})";
          code_.SetValue("TYPE_NAME", NormalizedName(*field.value.type.struct_def));
          if (field.IsRequired()) {
            code_ += "return {{TYPE_NAME}}(self._buf, o.take())";
          } else {
            code_ += "if o:";
            code_.IncrementIdentLevel();
            code_ += "return {{TYPE_NAME}}(self._buf, o.take())";
            code_.DecrementIdentLevel();
            code_ += "return None";
          }
          code_.DecrementIdentLevel();
          code_ += "";
        }
        break;
      }
      case BASE_TYPE_STRING:
        code_ += "fn {{NAME}}(self) -> StringRef:";
        code_.IncrementIdentLevel();
        code_ += "return flatbuffers.field_string(self._buf, int(self._pos), {{OFFSET}})";
        code_.DecrementIdentLevel();
        code_ += "";
        break;
      case BASE_TYPE_VECTOR: {
        auto vectortype = field.value.type.VectorType();
        code_.SetValue("SIZE", NumToString(InlineSize(vectortype)));
        if (vectortype.base_type == BASE_TYPE_STRUCT) {
          code_.SetValue("TYPE_NAME", NormalizedName(*field.value.type.struct_def));
          code_ += "fn {{NAME}}(self, i: Int) -> {{TYPE_NAME}}:";
          code_.IncrementIdentLevel();
          code_ += "var start = flatbuffers.field_vector(self._buf, int(self._pos), {{OFFSET}}) + i * {{SIZE}}";
          if (!(vectortype.struct_def->fixed)) {
            code_ += "start += flatbuffers.read_offset_as_int(self._buf, start)";
          }
          code_ += "return {{TYPE_NAME}}(self._buf, start)";
          code_.DecrementIdentLevel();
          code_ += "";
        } else {
          if (IsString(vectortype)) {
            code_.SetValue("TYPE_NAME", "StringRef");
            code_.SetValue("GET_FUNC", "flatbuffers.string");
          } else {
            code_.SetValue("TYPE_NAME", MojoType(vectortype));
            code_.SetValue("GET_FUNC", "flatbuffers.read[" + DTypeName(vectortype) + "]");
          }
          code_ += "fn {{NAME}}(self, i: Int) -> {{TYPE_NAME}}:";
          code_.IncrementIdentLevel();
          code_ += "return {{GET_FUNC}}(self._buf, flatbuffers.field_vector(self._buf, int(self._pos), {{OFFSET}}) + i * {{SIZE}})";
          code_.DecrementIdentLevel();
          code_ += "";
        }
        break;
      }
      case BASE_TYPE_UNION: {
        for (auto it = field.value.type.enum_def->Vals().begin();
             it != field.value.type.enum_def->Vals().end(); ++it) {
          auto &ev = **it;
          if (ev.IsNonZero()) {
            code_.SetValue("TYPE_NAME", NormalizedName(*ev.union_type.struct_def));
            code_.SetValue("EV", ev.name);
            code_ += "fn {{NAME}}_as_{{EV}}(self) -> {{TYPE_NAME}}:";
            code_.IncrementIdentLevel();
            code_ += "return {{TYPE_NAME}}(self._buf, flatbuffers.field_table(self._buf, int(self._pos), {{OFFSET}}).or_else(0))";
            code_.DecrementIdentLevel();
            code_ += "";
          }
        }
        break;
      }
      default: FLATBUFFERS_ASSERT(0);
    }
    if (IsVector(field.value.type)) {
      code_ += "fn {{NAME}}_length(self) -> Int:";
      code_.IncrementIdentLevel();
      code_ += "return flatbuffers.field_vector_len(self._buf, int(self._pos), {{OFFSET}})";
      code_.DecrementIdentLevel();
      code_ += "";
    }
  }

  // Generate table constructors, conditioned on its members' types.
  void GenTableBuilders(const StructDef &struct_def) {
    code_.SetValue("NAME", NormalizedName(struct_def));
    code_.SetValue("FIELD_COUNT", NumToString(struct_def.fields.vec.size()));
    code_ += "struct {{NAME}}Builder:";
    code_.IncrementIdentLevel();
    code_ += "@staticmethod";
    code_ += "fn start(inout builder: flatbuffers.Builder):";
    code_.IncrementIdentLevel();
    code_ += "builder.start_object({{FIELD_COUNT}})";
    code_.DecrementIdentLevel();
    code_ += "";
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;
      code_.SetValue("FIELD_NAME", NormalizedName(field));
      code_.SetValue("FIELD_TYPE", MojoTypeCreate(field.value.type));
      code_ += "@staticmethod";
      code_ += "fn add_{{FIELD_NAME}}(inout builder: flatbuffers.Builder, {{FIELD_NAME}}: {{FIELD_TYPE}}):";
      code_.IncrementIdentLevel();
      code_.SetValue("OFFSET", NumToString(it - struct_def.fields.vec.begin()));
      code_.SetValue("VALUE", NormalizedName(field));
      if (IsScalar(field.value.type.base_type) && !field.IsOptional()){
        code_.SetValue("DEF_CONST", field.value.constant);
        code_ += "if {{VALUE}} != {{DEF_CONST}}:";
        code_.IncrementIdentLevel();
      }
      code_ += "builder.prepend({{VALUE}})";
      code_ += "builder.slot({{OFFSET}})";
      if (IsScalar(field.value.type.base_type) && !field.IsOptional()){
        code_.DecrementIdentLevel();
      }
      code_.DecrementIdentLevel();
      code_ += "";
//      code += "    fn add_" + NormalizedName(field) + "(inout self, " +
//              NormalizedName(field) + ":" + MojoType(field.value.type) +
//              "):\n        b_.prepend" + GenMethod(field.value.type) + "Slot(" +
//              NumToString(offset) + ", " + NormalizedName(field);
//      if (IsScalar(field.value.type.base_type) && !field.IsOptional())
//        code += ", " + field.value.constant;
//      code += ")\n        return this\n";
    }

    code_ += "@staticmethod";
    code_ += "fn end(inout builder: flatbuffers.Builder) -> flatbuffers.Offset:";
    code_.IncrementIdentLevel();
    code_ += "return builder.end_object()";
    code_.DecrementIdentLevel();
    code_ += "";
    code_.DecrementIdentLevel();

//    code += "    fn end(owned self):\n        return b_.end_object()\n\n";
//    for (auto it = struct_def.fields.vec.begin();
//         it != struct_def.fields.vec.end(); ++it) {
//      auto &field = **it;
//      if (field.deprecated) continue;
//      if (IsVector(field.value.type)) {
//        code += "def " + NormalizedName(struct_def) + "Start" +
//                ConvertCase(NormalizedName(field), Case::kUpperCamel) +
//                "Vector(b_:flatbuffers.builder, n_:int):\n    b_.start_vector(";
//        auto vector_type = field.value.type.VectorType();
//        auto alignment = InlineAlignment(vector_type);
//        auto elem_size = InlineSize(vector_type);
//        code +=
//            NumToString(elem_size) + ", n_, " + NumToString(alignment) + ")\n";
//        if (vector_type.base_type != BASE_TYPE_STRUCT ||
//            !vector_type.struct_def->fixed) {
//          code += "def " + NormalizedName(struct_def) + "Create" +
//                  ConvertCase(NormalizedName(field), Case::kUpperCamel) +
//                  "Vector(b_:flatbuffers.builder, v_:[" +
//                  MojoType(vector_type) + "]):\n    b_.start_vector(" +
//                  NumToString(elem_size) + ", v_.length, " +
//                  NumToString(alignment) + ")\n    reverse(v_) e_: b_.prepend" +
//                  GenMethod(vector_type) +
//                  "(e_)\n    return b_.end_vector(v_.length)\n";
//        }
//        code += "\n";
//      }
//    }
  }

  void GenStructPreDecl(const StructDef &struct_def, std::string *code_ptr) {
//    if (struct_def.generated) return;
//    std::string &code = *code_ptr;
//    CheckNameSpace(struct_def, &code);
//    code += "class " + NormalizedName(struct_def) + "\n\n";
  }

  // Generate struct or table methods.
  void GenStruct(const StructDef &struct_def) {
    if (struct_def.generated) return;
//    CheckNameSpace(struct_def, &code);
    GenComment(struct_def.doc_comment);
    code_.SetValue("NAME", NormalizedName(struct_def));
    code_ += "@value";
    code_ += "struct {{NAME}}:";
    code_ += "    var _buf: UnsafePointer[UInt8]";
    code_ += "    var _pos: Int";
    code_ += "";
    code_.IncrementIdentLevel();
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;
      GenStructAccessor(struct_def, field);
    }
    if (!struct_def.fixed) {
      // Generate a special accessor for the table that has been declared as
      // the root type.
      code_.SetValue("NAME", NormalizedName(struct_def));
      code_ += "@staticmethod";
      code_ += "fn as_root(buf: UnsafePointer[UInt8]) -> {{NAME}}:";
      code_.IncrementIdentLevel();
      code_ += "return {{NAME}}(buf, flatbuffers.read_offset_as_int(buf, 0))";
      code_.DecrementIdentLevel();
      code_ += "";

      code_ += "@staticmethod";
      code_ += "fn build(";
      code_.IncrementIdentLevel();
      code_ += "inout builder: flatbuffers.Builder,";
      code_ += "*,";
      for (auto it = struct_def.fields.vec.begin();
           it != struct_def.fields.vec.end(); ++it) {
        auto &arg = **it;
        if (arg.deprecated) continue;
        code_.SetValue("ARG_NAME", NormalizedName(arg));
        code_.SetValue("ARG_TYPE", MojoArgType(arg.value.type));
        code_.SetValue("ARG_DEFAULT", MojoArgDefault(arg));
        code_ += "{{ARG_NAME}}: {{ARG_TYPE}} = {{ARG_DEFAULT}},";
      }
      code_.DecrementIdentLevel();
      code_ += ") -> flatbuffers.Offset:";
      code_.IncrementIdentLevel();

      for (auto it = struct_def.fields.vec.begin();
           it != struct_def.fields.vec.end(); ++it) {
        auto &arg = **it;
        if (arg.deprecated) continue;
        code_.SetValue("ARG_NAME", NormalizedName(arg));
        if (IsString(arg.value.type)) {
          code_ += "var _{{ARG_NAME}}: Optional[flatbuffers.Offset] = None";
          code_ += "if {{ARG_NAME}} is not None:";
          code_.IncrementIdentLevel();
          code_ += "_{{ARG_NAME}} = builder.prepend({{ARG_NAME}}.value())";
          code_.DecrementIdentLevel();
        } else if (IsVector(arg.value.type) && !IsVectorOfStruct(arg.value.type)) {
          code_ += "var _{{ARG_NAME}}: Optional[flatbuffers.Offset] = None";
          code_ += "if len({{ARG_NAME}}) > 0:";
          code_.IncrementIdentLevel();
          auto vector_type = arg.value.type.VectorType();
          code_.SetValue("ALIGNMENT", NumToString(InlineAlignment(vector_type)));
          code_.SetValue("SIZE", NumToString(InlineSize(vector_type)));
          code_ += "builder.start_vector({{SIZE}}, len({{ARG_NAME}}), {{ALIGNMENT}})";
          code_ += "for o in {{ARG_NAME}}.__reversed__():";
          code_.IncrementIdentLevel();
          if (IsEnum(vector_type)) {
            code_ += "builder.prepend(o[].value)";
          } else {
            code_ += "builder.prepend(o[])";
          }

          code_.DecrementIdentLevel();
          code_ += "_{{ARG_NAME}} = builder.end_vector(len({{ARG_NAME}}))";
          code_.DecrementIdentLevel();
          code_ += "";
        }
      }

      code_.SetValue("ARG_COUNT", NumToString(struct_def.fields.vec.size()));
      code_ += "builder.start_object({{ARG_COUNT}})";
      for (auto it = struct_def.fields.vec.begin();
           it != struct_def.fields.vec.end(); ++it) {
        auto &arg = **it;
        if (arg.deprecated) continue;
        code_.SetValue("ARG_NAME", NormalizedName(arg));
        code_.SetValue("ARG_VALUE", NormalizedName(arg));
        code_.SetValue("ARG_DEFAULT", MojoArgDefault(arg));
        code_.SetValue("OFFSET", NumToString(it - struct_def.fields.vec.begin()));
        if (IsScalar(arg.value.type.base_type) && !arg.IsOptional()){
          code_ += "if {{ARG_VALUE}} != {{ARG_DEFAULT}}:";
        } else {
          if (IsString(arg.value.type) || (IsVector(arg.value.type) && !IsVectorOfStruct(arg.value.type))) {
            code_.SetValue("ARG_VALUE", "_" + NormalizedName(arg));
          }
          code_ += "if {{ARG_VALUE}} is not None:";
        }
        code_.IncrementIdentLevel();
        if (IsStruct(arg.value.type)) {
          code_.SetValue("STRUCT_NAME", NormalizedName(*arg.value.type.struct_def));
          code_ += "{{STRUCT_NAME}}.build(";
          code_.IncrementIdentLevel();
          code_ += "builder,";
//          code_.SetValue("ARG_VALUE", NormalizedName(arg));
          for (auto it = arg.value.type.struct_def->fields.vec.begin();
               it != arg.value.type.struct_def->fields.vec.end(); ++it) {
            auto &field = **it;
            code_.SetValue("FIELD_NAME", NormalizedName(field));
            code_ += "{{FIELD_NAME}}={{ARG_VALUE}}.value().{{FIELD_NAME}},";
          }
          code_.DecrementIdentLevel();
          code_ += ")";
          code_ += "builder.slot({{OFFSET}})";
          code_.DecrementIdentLevel();
        } else {
          if (!(IsScalar(arg.value.type.base_type) && !arg.IsOptional())) {
            code_.SetValue("ARG_VALUE", code_.GetValue("ARG_VALUE") + ".value()");
          } else if (IsEnum(arg.value.type)) {
            code_.SetValue("ARG_VALUE", NormalizedName(arg) + ".value");
          }
          code_ += "builder.prepend({{ARG_VALUE}})";
          code_ += "builder.slot({{OFFSET}})";
          code_.DecrementIdentLevel();
        }
      }
      code_ += "return builder.end_object()";
      code_.DecrementIdentLevel();
      code_.DecrementIdentLevel();
      code_ += "";
      code_.DecrementIdentLevel();
    } else {
      GenStructBuilder(struct_def);
      code_.DecrementIdentLevel();
      GenStructVO(struct_def);
    }


//    if (struct_def.fixed) {
//      // create a struct constructor function
//      GenStructBuilder(struct_def);
//    } else {
//      // Create a set of functions that allow table construction.
//      GenTableBuilders(struct_def);
//    }
  }

  // Generate enum declarations.
  void GenEnum(const EnumDef &enum_def) {
    if (enum_def.generated) return;
//    CheckNameSpace(enum_def, &code);
    GenComment(enum_def.doc_comment);
    code_.SetValue("NAME", NormalizedName(enum_def));
    code_.SetValue("TYPE", EnumTypeName(enum_def.underlying_type));
    code_ += "@value";
    code_ += "struct {{NAME}}(EqualityComparable):";

    code_.IncrementIdentLevel();
    
    code_ += "var value: {{TYPE}}";
    code_ += "";

    for (auto it = enum_def.Vals().begin(); it != enum_def.Vals().end(); ++it) {
      auto &ev = **it;
      code_.SetValue("CASE_NAME", NormalizedName(ev));
      code_.SetValue("CASE_VALUE", enum_def.ToString(ev));
      GenComment(ev.doc_comment);
      code_ += "alias {{CASE_NAME}} = {{NAME}}({{CASE_VALUE}})";
    }
    code_ += "";
    code_ += "fn __eq__(self, other: {{NAME}}) -> Bool:";
    code_.IncrementIdentLevel();
    code_ += "return self.value == other.value";
    code_.DecrementIdentLevel();
    code_ += "";
    code_ += "fn __ne__(self, other: {{NAME}}) -> Bool:";
    code_.IncrementIdentLevel();
    code_ += "return self.value != other.value";
    code_.DecrementIdentLevel();
    code_.DecrementIdentLevel();
    code_ += "";
    code_ += "";
  }

  // Recursively generate arguments for a constructor, to deal with nested
  // structs.
  void StructBuilderArgs(const StructDef &struct_def, const char *nameprefix) {
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (IsStruct(field.value.type)) {
        // Generate arguments for a struct inside a struct. To ensure names
        // don't clash, and to make it obvious these arguments are constructing
        // a nested struct, prefix the name with the field name.
        StructBuilderArgs(*field.value.type.struct_def,
                          (nameprefix + (NormalizedName(field) + "_")).c_str());
      } else {
        code_.SetValue("ARG_NAME", nameprefix + NormalizedName(field));
        code_.SetValue("ARG_TYPE", MojoType(field.value.type));
        code_ += "{{ARG_NAME}}: {{ARG_TYPE}},";
//        std::string &code = *code_ptr;
//        code += ", " + (nameprefix + NormalizedName(field)) + ":" +
//                MojoType(field.value.type);
      }
    }
  }

  // Recursively generate struct construction statements and instert manual
  // padding.
  void StructBuilderBody(const StructDef &struct_def, const char *nameprefix) {
//    std::string &code = *code_ptr;
    code_.SetValue("MIN_ALIGN", NumToString(struct_def.minalign));
    code_.SetValue("BYTES", NumToString(struct_def.bytesize));
    code_ += "builder.prep({{MIN_ALIGN}}, {{BYTES}})";
    for (auto it = struct_def.fields.vec.rbegin();
         it != struct_def.fields.vec.rend(); ++it) {
      auto &field = **it;
      if (field.padding > 0)
//        code_.SetValue("PADDING", NumToString(field.padding));
        code_ += "builder.pad(" + NumToString(field.padding) + ")";
      if (IsStruct(field.value.type)) {
        StructBuilderBody(*field.value.type.struct_def,
                          (nameprefix + (NormalizedName(field) + "_")).c_str());
      } else {
        code_.SetValue("DTYPE", DTypeName(field.value.type));
        code_.SetValue("NAME", nameprefix + NormalizedName(field));
        if (IsEnum(field.value.type)){
          code_.SetValue("NAME", code_.GetValue("NAME") + ".value");
        }
        code_ += "builder.prepend[{{DTYPE}}]({{NAME}})";
      }
    }
  }

  void GenStructVO(const StructDef &struct_def) {
    code_ += "@value";
    code_.SetValue("NAME", NormalizedName(struct_def));
    code_ += "struct {{NAME}}VO:";
    code_.IncrementIdentLevel();
    for (auto it = struct_def.fields.vec.rbegin();
         it != struct_def.fields.vec.rend(); ++it) {
      auto &field = **it;
      code_.SetValue("FIELD_NAME", NormalizedName(field));
      code_.SetValue("FIELD_TYPE", MojoType(field.value.type));
      code_ += "var {{FIELD_NAME}}: {{FIELD_TYPE}}";
    }
    code_.DecrementIdentLevel();
    code_ += "";
  }

  // Create a struct with a builder and the struct's arguments.
  void GenStructBuilder(const StructDef &struct_def) {
    code_ += "@staticmethod";
    code_ += "fn build(";
    code_.IncrementIdentLevel();
    code_ += "inout builder: flatbuffers.Builder,";
    code_ += "*,";
    StructBuilderArgs(struct_def, "");
    code_.DecrementIdentLevel();
    code_ += "):";
    code_.IncrementIdentLevel();
    
    StructBuilderBody(struct_def, "");

    code_.DecrementIdentLevel();
    code_ += "";
//    std::string &code = *code_ptr;
//    code +=
//        "fn create_" + NormalizedName(struct_def) + "(b_: Builder";
//    StructBuilderArgs(struct_def, "", code_ptr);
//    code += ") -> Offset:\n";
//    StructBuilderBody(struct_def, "", code_ptr);
//    code += "    return b_.Offset()\n\n";
  }

  void CheckNameSpace(const Definition &def, std::string *code_ptr) {
//    auto ns = GetNameSpace(def);
//    if (ns == current_namespace_) return;
//    current_namespace_ = ns;
//    std::string &code = *code_ptr;
//    code += "namespace " + ns + "\n\n";
  }

  bool generate() {
    code_.Clear();
    code_ += std::string("# ") + FlatBuffersGeneratedWarning();
    code_ += "import flatbuffers";
    for (const auto &included : parser_.GetIncludedFiles()) {
      code_.SetValue("FILE_NAME", included.schema_name.substr(0, included.schema_name.size()-4) + parser_.opts.filename_suffix);
      code_ += "from {{FILE_NAME}} import *";
    }
    code_ += "";
    for (auto it = parser_.enums_.vec.begin(); it != parser_.enums_.vec.end();
         ++it) {
      auto &enum_def = **it;
      GenEnum(enum_def);
    }
//    for (auto it = parser_.structs_.vec.begin();
//         it != parser_.structs_.vec.end(); ++it) {
//      auto &struct_def = **it;
//      GenStructPreDecl(struct_def, &code);
//    }
    for (auto it = parser_.structs_.vec.begin();
         it != parser_.structs_.vec.end(); ++it) {
      auto &struct_def = **it;
      GenStruct(struct_def);
    }
    const auto filename = GeneratedFileName(path_, file_name_, parser_.opts);
    const auto final_code = code_.ToString();
    return SaveFile(filename.c_str(), final_code, false);
  }

 private:
  std::unordered_set<std::string> keywords_;
  std::string current_namespace_;
};

}  // namespace mojo

static bool GenerateMojo(const Parser &parser, const std::string &path,
                            const std::string &file_name) {
  mojo::MojoGenerator generator(parser, path, file_name);
  return generator.generate();
}

namespace {

class MojoCodeGenerator : public CodeGenerator {
 public:
  Status GenerateCode(const Parser &parser, const std::string &path,
                      const std::string &filename) override {
    if (!GenerateMojo(parser, path, filename)) { return Status::ERROR; }
    return Status::OK;
  }

  Status GenerateCode(const uint8_t *, int64_t,
                      const CodeGenOptions &) override {
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateMakeRule(const Parser &parser, const std::string &path,
                          const std::string &filename,
                          std::string &output) override {
    (void)parser;
    (void)path;
    (void)filename;
    (void)output;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateGrpcCode(const Parser &parser, const std::string &path,
                          const std::string &filename) override {
    (void)parser;
    (void)path;
    (void)filename;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateRootFile(const Parser &parser,
                          const std::string &path) override {
    (void)parser;
    (void)path;
    return Status::NOT_IMPLEMENTED;
  }

  bool IsSchemaOnly() const override { return true; }

  bool SupportsBfbsGeneration() const override { return false; }

  bool SupportsRootFileGeneration() const override { return false; }

  IDLOptions::Language Language() const override {
    return IDLOptions::kMojo;
  }

  std::string LanguageName() const override { return "Mojo"; }
};
}  // namespace

std::unique_ptr<CodeGenerator> NewMojoCodeGenerator() {
  return std::unique_ptr<MojoCodeGenerator>(new MojoCodeGenerator());
}

}  // namespace flatbuffers
