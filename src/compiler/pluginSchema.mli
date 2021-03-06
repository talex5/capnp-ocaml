type ro = Capnp.Message.ro
type rw = Capnp.Message.rw

module type S = sig
  type 'cap message_t

  module C2b2b_13688829037717245569 : C2b2b.S with
    type 'cap message_t = 'cap message_t

  type reader_t_Slot_14133145859926553711
  type builder_t_Slot_14133145859926553711
  type reader_t_Group_14626792032033250577
  type builder_t_Group_14626792032033250577
  type reader_t_Ordinal_13515537513213004774
  type builder_t_Ordinal_13515537513213004774
  type reader_t_Field_11145653318641710175
  type builder_t_Field_11145653318641710175
  type reader_t_Import_12560611460656617445
  type builder_t_Import_12560611460656617445
  type reader_t_RequestedFile_14981803260258615394
  type builder_t_RequestedFile_14981803260258615394
  type reader_t_CodeGeneratorRequest_13818529054586492878
  type builder_t_CodeGeneratorRequest_13818529054586492878
  type reader_t_NestedNode_16050641862814319170
  type builder_t_NestedNode_16050641862814319170
  type reader_t_Interface_16728431493453586831
  type builder_t_Interface_16728431493453586831
  type reader_t_Const_12793219851699983392
  type builder_t_Const_12793219851699983392
  type reader_t_Annotation_17011813041836786320
  type builder_t_Annotation_17011813041836786320
  type reader_t_Enum_13063450714778629528
  type builder_t_Enum_13063450714778629528
  type reader_t_Struct_11430331134483579957
  type builder_t_Struct_11430331134483579957
  type reader_t_Node_16610026722781537303
  type builder_t_Node_16610026722781537303
  type reader_t_Value_14853958794117909659
  type builder_t_Value_14853958794117909659
  type reader_t_Enumerant_10919677598968879693
  type builder_t_Enumerant_10919677598968879693
  type reader_t_Annotation_17422339044421236034
  type builder_t_Annotation_17422339044421236034
  type reader_t_Method_10736806783679155584
  type builder_t_Method_10736806783679155584
  type reader_t_Struct_12410354185295152851
  type builder_t_Struct_12410354185295152851
  type reader_t_List_9792858745991129751
  type builder_t_List_9792858745991129751
  type reader_t_Interface_17116997365232503999
  type builder_t_Interface_17116997365232503999
  type reader_t_Enum_11389172934837766057
  type builder_t_Enum_11389172934837766057
  type reader_t_Type_15020482145304562784
  type builder_t_Type_15020482145304562784
  module ElementSize_15102134695616452902 : sig
    type t =
      | Empty
      | Bit
      | Byte
      | TwoBytes
      | FourBytes
      | EightBytes
      | Pointer
      | InlineComposite
      | Undefined of int
  end

  module Reader : sig
    type array_t
    type builder_array_t
    type pointer_t
    module ElementSize : sig
      type t = ElementSize_15102134695616452902.t =
        | Empty
        | Bit
        | Byte
        | TwoBytes
        | FourBytes
        | EightBytes
        | Pointer
        | InlineComposite
        | Undefined of int
    end
    module Type : sig
      type t = reader_t_Type_15020482145304562784
      type builder_t = builder_t_Type_15020482145304562784
      module Enum : sig
        type t = reader_t_Enum_11389172934837766057
        type builder_t = builder_t_Enum_11389172934837766057
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Interface : sig
        type t = reader_t_Interface_17116997365232503999
        type builder_t = builder_t_Interface_17116997365232503999
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module List : sig
        type t = reader_t_List_9792858745991129751
        type builder_t = builder_t_List_9792858745991129751
        val has_element_type : t -> bool
        val element_type_get : t -> reader_t_Type_15020482145304562784
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Struct : sig
        type t = reader_t_Struct_12410354185295152851
        type builder_t = builder_t_Struct_12410354185295152851
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      type unnamed_union_t =
        | Void
        | Bool
        | Int8
        | Int16
        | Int32
        | Int64
        | Uint8
        | Uint16
        | Uint32
        | Uint64
        | Float32
        | Float64
        | Text
        | Data
        | List of List.t
        | Enum of Enum.t
        | Struct of Struct.t
        | Interface of Interface.t
        | AnyPointer
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Value : sig
      type t = reader_t_Value_14853958794117909659
      type builder_t = builder_t_Value_14853958794117909659
      type unnamed_union_t =
        | Void
        | Bool of bool
        | Int8 of int
        | Int16 of int
        | Int32 of int32
        | Int64 of int64
        | Uint8 of int
        | Uint16 of int
        | Uint32 of Uint32.t
        | Uint64 of Uint64.t
        | Float32 of float
        | Float64 of float
        | Text of string
        | Data of string
        | List of pointer_t
        | Enum of int
        | Struct of pointer_t
        | Interface
        | AnyPointer of pointer_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Annotation : sig
      type t = reader_t_Annotation_17422339044421236034
      type builder_t = builder_t_Annotation_17422339044421236034
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val has_value : t -> bool
      val value_get : t -> Value.t
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Method : sig
      type t = reader_t_Method_10736806783679155584
      type builder_t = builder_t_Method_10736806783679155584
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val param_struct_type_get : t -> Uint64.t
      val param_struct_type_get_int_exn : t -> int
      val result_struct_type_get : t -> Uint64.t
      val result_struct_type_get_int_exn : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Enumerant : sig
      type t = reader_t_Enumerant_10919677598968879693
      type builder_t = builder_t_Enumerant_10919677598968879693
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Field : sig
      type t = reader_t_Field_11145653318641710175
      type builder_t = builder_t_Field_11145653318641710175
      val no_discriminant : int
      module Ordinal : sig
        type t = reader_t_Ordinal_13515537513213004774
        type builder_t = builder_t_Ordinal_13515537513213004774
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Group : sig
        type t = reader_t_Group_14626792032033250577
        type builder_t = builder_t_Group_14626792032033250577
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Slot : sig
        type t = reader_t_Slot_14133145859926553711
        type builder_t = builder_t_Slot_14133145859926553711
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val has_type : t -> bool
        val type_get : t -> Type.t
        val has_default_value : t -> bool
        val default_value_get : t -> Value.t
        val had_explicit_default_get : t -> bool
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val has_name : t -> bool
      val name_get : t -> string
      val code_order_get : t -> int
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val discriminant_value_get : t -> int
      val ordinal_get : t -> Ordinal.t
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module Node : sig
      type t = reader_t_Node_16610026722781537303
      type builder_t = builder_t_Node_16610026722781537303
      module Struct : sig
        type t = reader_t_Struct_11430331134483579957
        type builder_t = builder_t_Struct_11430331134483579957
        val data_word_count_get : t -> int
        val pointer_count_get : t -> int
        val preferred_list_encoding_get : t -> ElementSize.t
        val is_group_get : t -> bool
        val discriminant_count_get : t -> int
        val discriminant_offset_get : t -> Uint32.t
        val discriminant_offset_get_int_exn : t -> int
        val has_fields : t -> bool
        val fields_get : t -> (ro, Field.t, array_t) Capnp.Array.t
        val fields_get_list : t -> Field.t list
        val fields_get_array : t -> Field.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Enum : sig
        type t = reader_t_Enum_13063450714778629528
        type builder_t = builder_t_Enum_13063450714778629528
        val has_enumerants : t -> bool
        val enumerants_get : t -> (ro, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_get_list : t -> Enumerant.t list
        val enumerants_get_array : t -> Enumerant.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Annotation : sig
        type t = reader_t_Annotation_17011813041836786320
        type builder_t = builder_t_Annotation_17011813041836786320
        val has_type : t -> bool
        val type_get : t -> Type.t
        val targets_file_get : t -> bool
        val targets_const_get : t -> bool
        val targets_enum_get : t -> bool
        val targets_enumerant_get : t -> bool
        val targets_struct_get : t -> bool
        val targets_field_get : t -> bool
        val targets_union_get : t -> bool
        val targets_group_get : t -> bool
        val targets_interface_get : t -> bool
        val targets_method_get : t -> bool
        val targets_param_get : t -> bool
        val targets_annotation_get : t -> bool
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Const : sig
        type t = reader_t_Const_12793219851699983392
        type builder_t = builder_t_Const_12793219851699983392
        val has_type : t -> bool
        val type_get : t -> Type.t
        val has_value : t -> bool
        val value_get : t -> Value.t
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module Interface : sig
        type t = reader_t_Interface_16728431493453586831
        type builder_t = builder_t_Interface_16728431493453586831
        val has_methods : t -> bool
        val methods_get : t -> (ro, Method.t, array_t) Capnp.Array.t
        val methods_get_list : t -> Method.t list
        val methods_get_array : t -> Method.t array
        val has_extends : t -> bool
        val extends_get : t -> (ro, Uint64.t, array_t) Capnp.Array.t
        val extends_get_list : t -> Uint64.t list
        val extends_get_array : t -> Uint64.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      module NestedNode : sig
        type t = reader_t_NestedNode_16050641862814319170
        type builder_t = builder_t_NestedNode_16050641862814319170
        val has_name : t -> bool
        val name_get : t -> string
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      type unnamed_union_t =
        | File
        | Struct of Struct.t
        | Enum of Enum.t
        | Interface of Interface.t
        | Const of Const.t
        | Annotation of Annotation.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val has_display_name : t -> bool
      val display_name_get : t -> string
      val display_name_prefix_length_get : t -> Uint32.t
      val display_name_prefix_length_get_int_exn : t -> int
      val scope_id_get : t -> Uint64.t
      val scope_id_get_int_exn : t -> int
      val has_nested_nodes : t -> bool
      val nested_nodes_get : t -> (ro, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_get_list : t -> NestedNode.t list
      val nested_nodes_get_array : t -> NestedNode.t array
      val has_annotations : t -> bool
      val annotations_get : t -> (ro, reader_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_get_list : t -> reader_t_Annotation_17422339044421236034 list
      val annotations_get_array : t -> reader_t_Annotation_17422339044421236034 array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
    module CodeGeneratorRequest : sig
      type t = reader_t_CodeGeneratorRequest_13818529054586492878
      type builder_t = builder_t_CodeGeneratorRequest_13818529054586492878
      module RequestedFile : sig
        type t = reader_t_RequestedFile_14981803260258615394
        type builder_t = builder_t_RequestedFile_14981803260258615394
        module Import : sig
          type t = reader_t_Import_12560611460656617445
          type builder_t = builder_t_Import_12560611460656617445
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val has_name : t -> bool
          val name_get : t -> string
          val of_message : 'cap message_t -> t
          val of_builder : builder_t -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val has_filename : t -> bool
        val filename_get : t -> string
        val has_imports : t -> bool
        val imports_get : t -> (ro, Import.t, array_t) Capnp.Array.t
        val imports_get_list : t -> Import.t list
        val imports_get_array : t -> Import.t array
        val of_message : 'cap message_t -> t
        val of_builder : builder_t -> t
      end
      val has_nodes : t -> bool
      val nodes_get : t -> (ro, Node.t, array_t) Capnp.Array.t
      val nodes_get_list : t -> Node.t list
      val nodes_get_array : t -> Node.t array
      val has_requested_files : t -> bool
      val requested_files_get : t -> (ro, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_get_list : t -> RequestedFile.t list
      val requested_files_get_array : t -> RequestedFile.t array
      val of_message : 'cap message_t -> t
      val of_builder : builder_t -> t
    end
  end

  module Builder : sig
    type array_t = Reader.builder_array_t
    type reader_array_t = Reader.array_t
    type pointer_t
    module ElementSize : sig
      type t = ElementSize_15102134695616452902.t =
        | Empty
        | Bit
        | Byte
        | TwoBytes
        | FourBytes
        | EightBytes
        | Pointer
        | InlineComposite
        | Undefined of int
    end
    module Type : sig
      type t = builder_t_Type_15020482145304562784
      type reader_t = reader_t_Type_15020482145304562784
      module Enum : sig
        type t = builder_t_Enum_11389172934837766057
        type reader_t = reader_t_Enum_11389172934837766057
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Interface : sig
        type t = builder_t_Interface_17116997365232503999
        type reader_t = reader_t_Interface_17116997365232503999
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module List : sig
        type t = builder_t_List_9792858745991129751
        type reader_t = reader_t_List_9792858745991129751
        val has_element_type : t -> bool
        val element_type_get : t -> builder_t_Type_15020482145304562784
        val element_type_set_reader : t -> reader_t_Type_15020482145304562784 -> builder_t_Type_15020482145304562784
        val element_type_set_builder : t -> builder_t_Type_15020482145304562784 -> builder_t_Type_15020482145304562784
        val element_type_init : t -> builder_t_Type_15020482145304562784
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Struct : sig
        type t = builder_t_Struct_12410354185295152851
        type reader_t = reader_t_Struct_12410354185295152851
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      type unnamed_union_t =
        | Void
        | Bool
        | Int8
        | Int16
        | Int32
        | Int64
        | Uint8
        | Uint16
        | Uint32
        | Uint64
        | Float32
        | Float64
        | Text
        | Data
        | List of List.t
        | Enum of Enum.t
        | Struct of Struct.t
        | Interface of Interface.t
        | AnyPointer
        | Undefined of int
      val get : t -> unnamed_union_t
      val void_set : t -> unit
      val bool_set : t -> unit
      val int8_set : t -> unit
      val int16_set : t -> unit
      val int32_set : t -> unit
      val int64_set : t -> unit
      val uint8_set : t -> unit
      val uint16_set : t -> unit
      val uint32_set : t -> unit
      val uint64_set : t -> unit
      val float32_set : t -> unit
      val float64_set : t -> unit
      val text_set : t -> unit
      val data_set : t -> unit
      val list_init : t -> List.t
      val enum_init : t -> Enum.t
      val struct_init : t -> Struct.t
      val interface_init : t -> Interface.t
      val any_pointer_set : t -> unit
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Value : sig
      type t = builder_t_Value_14853958794117909659
      type reader_t = reader_t_Value_14853958794117909659
      type unnamed_union_t =
        | Void
        | Bool of bool
        | Int8 of int
        | Int16 of int
        | Int32 of int32
        | Int64 of int64
        | Uint8 of int
        | Uint16 of int
        | Uint32 of Uint32.t
        | Uint64 of Uint64.t
        | Float32 of float
        | Float64 of float
        | Text of string
        | Data of string
        | List of pointer_t
        | Enum of int
        | Struct of pointer_t
        | Interface
        | AnyPointer of pointer_t
        | Undefined of int
      val get : t -> unnamed_union_t
      val void_set : t -> unit
      val bool_set : t -> bool -> unit
      val int8_set_exn : t -> int -> unit
      val int16_set_exn : t -> int -> unit
      val int32_set : t -> int32 -> unit
      val int32_set_int_exn : t -> int -> unit
      val int64_set : t -> int64 -> unit
      val int64_set_int : t -> int -> unit
      val uint8_set_exn : t -> int -> unit
      val uint16_set_exn : t -> int -> unit
      val uint32_set : t -> Uint32.t -> unit
      val uint32_set_int_exn : t -> int -> unit
      val uint64_set : t -> Uint64.t -> unit
      val uint64_set_int_exn : t -> int -> unit
      val float32_set : t -> float -> unit
      val float64_set : t -> float -> unit
      val text_set : t -> string -> unit
      val data_set : t -> string -> unit
      val list_set : t -> pointer_t -> pointer_t
      val enum_set_exn : t -> int -> unit
      val struct_set : t -> pointer_t -> pointer_t
      val interface_set : t -> unit
      val any_pointer_set : t -> pointer_t -> pointer_t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Annotation : sig
      type t = builder_t_Annotation_17422339044421236034
      type reader_t = reader_t_Annotation_17422339044421236034
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val has_value : t -> bool
      val value_get : t -> Value.t
      val value_set_reader : t -> Value.reader_t -> Value.t
      val value_set_builder : t -> Value.t -> Value.t
      val value_init : t -> Value.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Method : sig
      type t = builder_t_Method_10736806783679155584
      type reader_t = reader_t_Method_10736806783679155584
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val param_struct_type_get : t -> Uint64.t
      val param_struct_type_get_int_exn : t -> int
      val param_struct_type_set : t -> Uint64.t -> unit
      val param_struct_type_set_int_exn : t -> int -> unit
      val result_struct_type_get : t -> Uint64.t
      val result_struct_type_get_int_exn : t -> int
      val result_struct_type_set : t -> Uint64.t -> unit
      val result_struct_type_set_int_exn : t -> int -> unit
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Array.t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_list : t -> Annotation.t list -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_array : t -> Annotation.t array -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Enumerant : sig
      type t = builder_t_Enumerant_10919677598968879693
      type reader_t = reader_t_Enumerant_10919677598968879693
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Array.t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_list : t -> Annotation.t list -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_array : t -> Annotation.t array -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Field : sig
      type t = builder_t_Field_11145653318641710175
      type reader_t = reader_t_Field_11145653318641710175
      val no_discriminant : int
      module Ordinal : sig
        type t = builder_t_Ordinal_13515537513213004774
        type reader_t = reader_t_Ordinal_13515537513213004774
        type unnamed_union_t =
          | Implicit
          | Explicit of int
          | Undefined of int
        val get : t -> unnamed_union_t
        val implicit_set : t -> unit
        val explicit_set_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Group : sig
        type t = builder_t_Group_14626792032033250577
        type reader_t = reader_t_Group_14626792032033250577
        val type_id_get : t -> Uint64.t
        val type_id_get_int_exn : t -> int
        val type_id_set : t -> Uint64.t -> unit
        val type_id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Slot : sig
        type t = builder_t_Slot_14133145859926553711
        type reader_t = reader_t_Slot_14133145859926553711
        val offset_get : t -> Uint32.t
        val offset_get_int_exn : t -> int
        val offset_set : t -> Uint32.t -> unit
        val offset_set_int_exn : t -> int -> unit
        val has_type : t -> bool
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val has_default_value : t -> bool
        val default_value_get : t -> Value.t
        val default_value_set_reader : t -> Value.reader_t -> Value.t
        val default_value_set_builder : t -> Value.t -> Value.t
        val default_value_init : t -> Value.t
        val had_explicit_default_get : t -> bool
        val had_explicit_default_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      type unnamed_union_t =
        | Slot of Slot.t
        | Group of Group.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val slot_init : t -> Slot.t
      val group_init : t -> Group.t
      val has_name : t -> bool
      val name_get : t -> string
      val name_set : t -> string -> unit
      val code_order_get : t -> int
      val code_order_set_exn : t -> int -> unit
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_get_list : t -> Annotation.t list
      val annotations_get_array : t -> Annotation.t array
      val annotations_set : t -> (rw, Annotation.t, array_t) Capnp.Array.t -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_list : t -> Annotation.t list -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_set_array : t -> Annotation.t array -> (rw, Annotation.t, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, Annotation.t, array_t) Capnp.Array.t
      val discriminant_value_get : t -> int
      val discriminant_value_set_exn : t -> int -> unit
      val ordinal_get : t -> Ordinal.t
      val ordinal_init : t -> Ordinal.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module Node : sig
      type t = builder_t_Node_16610026722781537303
      type reader_t = reader_t_Node_16610026722781537303
      module Struct : sig
        type t = builder_t_Struct_11430331134483579957
        type reader_t = reader_t_Struct_11430331134483579957
        val data_word_count_get : t -> int
        val data_word_count_set_exn : t -> int -> unit
        val pointer_count_get : t -> int
        val pointer_count_set_exn : t -> int -> unit
        val preferred_list_encoding_get : t -> ElementSize.t
        val preferred_list_encoding_set : t -> ElementSize.t -> unit
        val preferred_list_encoding_set_unsafe : t -> ElementSize.t -> unit
        val is_group_get : t -> bool
        val is_group_set : t -> bool -> unit
        val discriminant_count_get : t -> int
        val discriminant_count_set_exn : t -> int -> unit
        val discriminant_offset_get : t -> Uint32.t
        val discriminant_offset_get_int_exn : t -> int
        val discriminant_offset_set : t -> Uint32.t -> unit
        val discriminant_offset_set_int_exn : t -> int -> unit
        val has_fields : t -> bool
        val fields_get : t -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_get_list : t -> Field.t list
        val fields_get_array : t -> Field.t array
        val fields_set : t -> (rw, Field.t, array_t) Capnp.Array.t -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_set_list : t -> Field.t list -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_set_array : t -> Field.t array -> (rw, Field.t, array_t) Capnp.Array.t
        val fields_init : t -> int -> (rw, Field.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Enum : sig
        type t = builder_t_Enum_13063450714778629528
        type reader_t = reader_t_Enum_13063450714778629528
        val has_enumerants : t -> bool
        val enumerants_get : t -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_get_list : t -> Enumerant.t list
        val enumerants_get_array : t -> Enumerant.t array
        val enumerants_set : t -> (rw, Enumerant.t, array_t) Capnp.Array.t -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_set_list : t -> Enumerant.t list -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_set_array : t -> Enumerant.t array -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val enumerants_init : t -> int -> (rw, Enumerant.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Annotation : sig
        type t = builder_t_Annotation_17011813041836786320
        type reader_t = reader_t_Annotation_17011813041836786320
        val has_type : t -> bool
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val targets_file_get : t -> bool
        val targets_file_set : t -> bool -> unit
        val targets_const_get : t -> bool
        val targets_const_set : t -> bool -> unit
        val targets_enum_get : t -> bool
        val targets_enum_set : t -> bool -> unit
        val targets_enumerant_get : t -> bool
        val targets_enumerant_set : t -> bool -> unit
        val targets_struct_get : t -> bool
        val targets_struct_set : t -> bool -> unit
        val targets_field_get : t -> bool
        val targets_field_set : t -> bool -> unit
        val targets_union_get : t -> bool
        val targets_union_set : t -> bool -> unit
        val targets_group_get : t -> bool
        val targets_group_set : t -> bool -> unit
        val targets_interface_get : t -> bool
        val targets_interface_set : t -> bool -> unit
        val targets_method_get : t -> bool
        val targets_method_set : t -> bool -> unit
        val targets_param_get : t -> bool
        val targets_param_set : t -> bool -> unit
        val targets_annotation_get : t -> bool
        val targets_annotation_set : t -> bool -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Const : sig
        type t = builder_t_Const_12793219851699983392
        type reader_t = reader_t_Const_12793219851699983392
        val has_type : t -> bool
        val type_get : t -> Type.t
        val type_set_reader : t -> Type.reader_t -> Type.t
        val type_set_builder : t -> Type.t -> Type.t
        val type_init : t -> Type.t
        val has_value : t -> bool
        val value_get : t -> Value.t
        val value_set_reader : t -> Value.reader_t -> Value.t
        val value_set_builder : t -> Value.t -> Value.t
        val value_init : t -> Value.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module Interface : sig
        type t = builder_t_Interface_16728431493453586831
        type reader_t = reader_t_Interface_16728431493453586831
        val has_methods : t -> bool
        val methods_get : t -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_get_list : t -> Method.t list
        val methods_get_array : t -> Method.t array
        val methods_set : t -> (rw, Method.t, array_t) Capnp.Array.t -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_set_list : t -> Method.t list -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_set_array : t -> Method.t array -> (rw, Method.t, array_t) Capnp.Array.t
        val methods_init : t -> int -> (rw, Method.t, array_t) Capnp.Array.t
        val has_extends : t -> bool
        val extends_get : t -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_get_list : t -> Uint64.t list
        val extends_get_array : t -> Uint64.t array
        val extends_set : t -> (rw, Uint64.t, array_t) Capnp.Array.t -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_set_list : t -> Uint64.t list -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_set_array : t -> Uint64.t array -> (rw, Uint64.t, array_t) Capnp.Array.t
        val extends_init : t -> int -> (rw, Uint64.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      module NestedNode : sig
        type t = builder_t_NestedNode_16050641862814319170
        type reader_t = reader_t_NestedNode_16050641862814319170
        val has_name : t -> bool
        val name_get : t -> string
        val name_set : t -> string -> unit
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      type unnamed_union_t =
        | File
        | Struct of Struct.t
        | Enum of Enum.t
        | Interface of Interface.t
        | Const of Const.t
        | Annotation of Annotation.t
        | Undefined of int
      val get : t -> unnamed_union_t
      val file_set : t -> unit
      val struct_init : t -> Struct.t
      val enum_init : t -> Enum.t
      val interface_init : t -> Interface.t
      val const_init : t -> Const.t
      val annotation_init : t -> Annotation.t
      val id_get : t -> Uint64.t
      val id_get_int_exn : t -> int
      val id_set : t -> Uint64.t -> unit
      val id_set_int_exn : t -> int -> unit
      val has_display_name : t -> bool
      val display_name_get : t -> string
      val display_name_set : t -> string -> unit
      val display_name_prefix_length_get : t -> Uint32.t
      val display_name_prefix_length_get_int_exn : t -> int
      val display_name_prefix_length_set : t -> Uint32.t -> unit
      val display_name_prefix_length_set_int_exn : t -> int -> unit
      val scope_id_get : t -> Uint64.t
      val scope_id_get_int_exn : t -> int
      val scope_id_set : t -> Uint64.t -> unit
      val scope_id_set_int_exn : t -> int -> unit
      val has_nested_nodes : t -> bool
      val nested_nodes_get : t -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_get_list : t -> NestedNode.t list
      val nested_nodes_get_array : t -> NestedNode.t array
      val nested_nodes_set : t -> (rw, NestedNode.t, array_t) Capnp.Array.t -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_set_list : t -> NestedNode.t list -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_set_array : t -> NestedNode.t array -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val nested_nodes_init : t -> int -> (rw, NestedNode.t, array_t) Capnp.Array.t
      val has_annotations : t -> bool
      val annotations_get : t -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_get_list : t -> builder_t_Annotation_17422339044421236034 list
      val annotations_get_array : t -> builder_t_Annotation_17422339044421236034 array
      val annotations_set : t -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_set_list : t -> builder_t_Annotation_17422339044421236034 list -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_set_array : t -> builder_t_Annotation_17422339044421236034 array -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val annotations_init : t -> int -> (rw, builder_t_Annotation_17422339044421236034, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
    module CodeGeneratorRequest : sig
      type t = builder_t_CodeGeneratorRequest_13818529054586492878
      type reader_t = reader_t_CodeGeneratorRequest_13818529054586492878
      module RequestedFile : sig
        type t = builder_t_RequestedFile_14981803260258615394
        type reader_t = reader_t_RequestedFile_14981803260258615394
        module Import : sig
          type t = builder_t_Import_12560611460656617445
          type reader_t = reader_t_Import_12560611460656617445
          val id_get : t -> Uint64.t
          val id_get_int_exn : t -> int
          val id_set : t -> Uint64.t -> unit
          val id_set_int_exn : t -> int -> unit
          val has_name : t -> bool
          val name_get : t -> string
          val name_set : t -> string -> unit
          val of_message : rw message_t -> t
          val to_message : t -> rw message_t
          val to_reader : t -> reader_t
          val init_root : ?message_size:int -> unit -> t
        end
        val id_get : t -> Uint64.t
        val id_get_int_exn : t -> int
        val id_set : t -> Uint64.t -> unit
        val id_set_int_exn : t -> int -> unit
        val has_filename : t -> bool
        val filename_get : t -> string
        val filename_set : t -> string -> unit
        val has_imports : t -> bool
        val imports_get : t -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_get_list : t -> Import.t list
        val imports_get_array : t -> Import.t array
        val imports_set : t -> (rw, Import.t, array_t) Capnp.Array.t -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_set_list : t -> Import.t list -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_set_array : t -> Import.t array -> (rw, Import.t, array_t) Capnp.Array.t
        val imports_init : t -> int -> (rw, Import.t, array_t) Capnp.Array.t
        val of_message : rw message_t -> t
        val to_message : t -> rw message_t
        val to_reader : t -> reader_t
        val init_root : ?message_size:int -> unit -> t
      end
      val has_nodes : t -> bool
      val nodes_get : t -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_get_list : t -> Node.t list
      val nodes_get_array : t -> Node.t array
      val nodes_set : t -> (rw, Node.t, array_t) Capnp.Array.t -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_set_list : t -> Node.t list -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_set_array : t -> Node.t array -> (rw, Node.t, array_t) Capnp.Array.t
      val nodes_init : t -> int -> (rw, Node.t, array_t) Capnp.Array.t
      val has_requested_files : t -> bool
      val requested_files_get : t -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_get_list : t -> RequestedFile.t list
      val requested_files_get_array : t -> RequestedFile.t array
      val requested_files_set : t -> (rw, RequestedFile.t, array_t) Capnp.Array.t -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_set_list : t -> RequestedFile.t list -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_set_array : t -> RequestedFile.t array -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val requested_files_init : t -> int -> (rw, RequestedFile.t, array_t) Capnp.Array.t
      val of_message : rw message_t -> t
      val to_message : t -> rw message_t
      val to_reader : t -> reader_t
      val init_root : ?message_size:int -> unit -> t
    end
  end
end

module Make (MessageWrapper : Capnp.MessageSig.S) :
  (S with type 'cap message_t = 'cap MessageWrapper.Message.t
    and type Reader.pointer_t = ro MessageWrapper.Slice.t option
    and type Builder.pointer_t = rw MessageWrapper.Slice.t
    and module C2b2b_13688829037717245569 = C2b2b.Make(MessageWrapper)
)

