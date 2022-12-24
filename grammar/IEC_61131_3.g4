/**
 * Grammar file for IEC61131-3 2013 standard
 */
grammar IEC_61131_3;

// Table 71 - 72 - Language Structured Text (ST)
stmt_list : ( stmt ? ';' )*;
stmt : assign_stmt | subprog_ctrl_stmt | selection_stmt | iteration_stmt;
assign_stmt : ( variable ':=' expression ) | ref_assign | assignment_attempt;
subprog_ctrl_stmt : func_call | invocation | 'SUPER' '(' ')' | 'RETURN';
selection_stmt : if_stmt | case_stmt;
iteration_stmt : for_stmt | while_stmt | repeat_stmt | 'EXIT' | 'CONTINUE';
if_stmt : 'IF' expression 'THEN' stmt_list ( 'ELSIF' expression 'THEN' stmt_list )* ( 'ELSE' stmt_list )? 'END_IF';
case_stmt : 'CASE' expression 'OF' case_selection + ( 'ELSE' stmt_list )? 'END_CASE';
for_stmt : 'FOR' control_variable ':=' for_list 'DO' stmt_list 'END_FOR';
while_stmt : 'WHILE' expression 'DO' stmt_list 'END_WHILE';
repeat_stmt : 'REPEAT' stmt_list 'UNTIL' expression 'END_REPEAT';
expression : xor_expr ( 'OR' xor_expr )*;
constant_expr : expression;                                     // a constant expression must evaluate to a constant value at compile time
xor_expr : and_expr ( 'XOR' and_expr )*;
and_expr : compare_expr ( ( '&' | 'AND' ) compare_expr )*;
compare_expr : ( equ_expr ( ( '=' | '<>' ) equ_expr )* );
equ_expr : add_expr ( ( '<' | '>' | '<=' | '>=' ) add_expr )*;
add_expr : term ( ( '+' | '-' ) term )*;
term : power_expr ( '*' | '/' | 'MOD' power_expr )*;
power_expr : unary_expr ( '**' unary_expr )*;
unary_expr : '-' | '+' | 'NOT' ? primary_expr;
primary_expr : constant | enum_value | variable_access | func_call | ref_value| '(' expression ')';
variable_access : variable MULTIBIT_PART_ACCESS ?;
func_call : func_access '(' ( param_assign ( ',' param_assign )* )? ')';
assignment_attempt : ( ref_name | ref_deref ) '?=' ( ref_name | ref_deref | ref_value );
invocation  : ( fb_instance_name | method_name | 'THIS'
            | ( ( 'THIS' '.' )? ( ( ( fb_instance_name | class_instance_name ) '.' )+ ) method_name ) )
            '(' ( param_assign ( ',' param_assign )* )? ')';
param_assign : ( ( variable_name ':=' )? expression ) | ref_assign | ( 'NOT' ? variable_name '=>' variable );
case_selection : case_list ':' stmt_list;
case_list : case_list_elem ( ',' case_list_elem )*;
case_list_elem : subrange | constant_expr;
control_variable : ID;
for_list : expression 'TO' expression ( 'BY' expression )?;


// Table 67 - 70 - Instruction List (IL)
instruction_list : il_instruction +;
il_instruction  : ( il_label ':' )? ( il_simple_operation | il_expr | il_jump_operation
                | il_invocation | il_formal_func_call
                | IL_RETURN_OPERATOR )? EOL +;
il_simple_inst : il_simple_operation | il_expr | il_formal_func_call;
il_label : ID;
il_simple_operation : IL_SIMPLE_OPERATOR il_operand ? | func_access il_operand_list ?;
il_expr : IL_EXPR_OPERATOR '(' il_operand ? EOL + il_simple_inst_list ? ')';
il_jump_operation : IL_JUMP_OPERATOR il_label;
il_invocation   : IL_CALL_OPERATOR ((( fb_instance_name | func_name | method_name | 'THIS '
                | ( ( 'THIS' '.' ( ( fb_instance_name | class_instance_name ) '.' )* ) method_name ) )
                ( '(' ( ( EOL + il_param_list ? ) | il_operand_list ? ) ')' )? ) | 'SUPER' '(' ')' );
il_formal_func_call : func_access '(' EOL + il_param_list ? ')';
il_operand : constant | enum_value | variable_access;
il_operand_list : il_operand ( ',' il_operand )*;
il_simple_inst_list : il_simple_instruction +;
il_simple_instruction : ( il_simple_operation | il_expr | il_formal_func_call ) EOL +;
il_param_list : il_param_inst * il_param_last_inst;
il_param_inst : ( il_param_assign | il_param_out_assign ) ',' EOL +;
il_param_last_inst : ( il_param_assign | il_param_out_assign ) EOL +;
il_param_assign : il_assignment ( il_operand | ( '(' EOL + il_simple_inst_list ')' ) );
il_param_out_assign : il_assign_out_operator variable_access;
il_assignment : variable_name ':=';
il_assign_out_operator : 'NOT' ? variable_name '=>';


// Table 62 - Configuration and resource declaration
config_name : ID;
resource_type_name : ID;
config_decl : 'CONFIGURATION' config_name global_var_decls ?
            ( single_resource_decl | resource_decl + ) access_decls ? config_init ?
            'END_CONFIGURATION';
resource_decl   : 'RESOURCE' resource_name 'ON' resource_type_name
                global_var_decls ? single_resource_decl
                'END_RESOURCE';
single_resource_decl : ( task_config ';' )* ( prog_config ';' )+;
resource_name : ID;
access_decls : 'VAR_ACCESS' ( access_decl ';' )* 'END_VAR';
access_decl : access_name ':' access_path ':' data_type_access access_direction ?;
access_path : ( resource_name '.' )? DIRECT_VARIABLE
            | ( resource_name '.' )? ( prog_name '.' )?
            ( ( fb_instance_name | class_instance_name ) '.' )* symbolic_variable;
global_var_access : ( resource_name '.' )? global_var_name ( '.' struct_elem_name )?;
access_name : ID;
prog_output_access : prog_name '.' symbolic_variable;
prog_name : ID;
access_direction : 'READ_WRITE' | 'READ_ONLY';
task_config : 'TASK' task_name task_init;
task_name : ID;
task_init   : '(' ( 'SINGLE' ':=' data_source ',' )?
            ( 'INTERVAL' ':=' data_source ',' )?
            'PRIORITY' ':=' UNSIGNED_INT ')';
data_source : constant | global_var_access | prog_output_access | DIRECT_VARIABLE;
prog_config : 'PROGRAM' ( 'RETAIN' | 'NON_RETAIN' )? prog_name ( 'WITH' task_name )? ':'
prog_type_access ( '(' prog_conf_elems ')' )?;
prog_conf_elems : prog_conf_elem ( ',' prog_conf_elem )*;
prog_conf_elem : fb_task | prog_cnxn;
fb_task : fb_instance_name 'WITH' task_name;
prog_cnxn : symbolic_variable ':=' prog_data_source | symbolic_variable '=>' data_sink;
prog_data_source : constant | enum_value | global_var_access | DIRECT_VARIABLE;
data_sink : global_var_access | DIRECT_VARIABLE;
config_init : 'VAR_CONFIG' ( config_inst_init ';' )* 'END_VAR';
config_inst_init    : resource_name '.' prog_name '.' ( ( fb_instance_name | class_instance_name ) '.' )*
                    ( variable_name located_at ? ':' loc_var_spec_init
                    | ( ( fb_instance_name ':' fb_type_access )
                    | ( class_instance_name ':' class_type_access ) ) ':=' struct_init );

// Table 54 - 61 - Sequential Function Chart (SFC)
sfc :sfc_network +;
sfc_network : initial_step ( step | transition | action )*;
initial_step : 'INITIAL_STEP' step_name ':' ( action_association ';' )* 'END_STEP';
step : 'STEP' step_name ':' ( action_association ';' )* 'END_STEP';
step_name : ID;
action_association : action_name '(' action_qualifier ? ( ',' indicator_name )* ')';
action_name : ID;
action_qualifier : 'N' | 'R' | 'S' | 'P' | ( ( 'L' | 'D' | 'SD' | 'DS' | 'SL' ) ',' action_time );
action_time : DURATION | variable_name;
indicator_name : variable_name;
transition  : 'TRANSITION' transition_name ? ( '(' 'PRIORITY' ':=' UNSIGNED_INT ')' )?
            'FROM' steps 'TO' steps ':' transition_cond 'END_TRANSITION';
transition_name : ID;
steps : step_name | '(' step_name ( ',' step_name )+ ')';
//transition_cond : ':=' expression ';' | ':' ( fbd_network | ld_rung ) | ':=' il_simple_inst;
transition_cond : ':=' expression ';' | ':=' il_simple_inst;
action : 'ACTION' action_name ':' fb_body 'END_ACTION';


// Table 47 - Program declaration
prog_decl   : 'PROGRAM' prog_type_name
            ( io_var_decls | func_var_decls | temp_var_decls | other_var_decls
            | loc_var_decls | prog_access_decls )* fb_body 'END_PROGRAM';
prog_type_name : ID;
prog_type_access : ( namespace_name '.' )* prog_type_name;
prog_access_decls : 'VAR_ACCESS' ( prog_access_decl ';' )* 'END_VAR';
prog_access_decl : access_name ':' symbolic_variable MULTIBIT_PART_ACCESS ?
                ':' data_type_access access_direction ?;


// Table 48 - Class
// Table 50 Textual call of methods – Formal and non-formal parameter list
class_decl  : 'CLASS' ( 'FINAL' | 'ABSTRACT' )? class_type_name using_directive *
            ( 'EXTENDS' class_type_access )? ( 'IMPLEMENTS' interface_name_list )?
            ( func_var_decls | other_var_decls )* ( method_decl )* 'END_CLASS';
class_type_name : ID;
class_type_access : ( namespace_name '.' )* class_type_name;
class_name : ID;
class_instance_name : ( namespace_name '.' )* class_name '^' *;
interface_decl  : 'INTERFACE' interface_type_name using_directive *
                ( 'EXTENDS' interface_name_list )?method_prototype * 'END_INTERFACE';
method_prototype : 'METHOD' method_name ( ':' data_type_access )? io_var_decls * 'END_METHOD';
interface_spec_init : variable_list ( ':=' interface_value )?;
interface_value : symbolic_variable | fb_instance_name | class_instance_name | 'NULL';
interface_name_list : interface_type_access ( ',' interface_type_access )*;
interface_type_name : ID;
interface_type_access : ( namespace_name '.' )* interface_type_name;
interface_name : ID;
access_spec : 'PUBLIC' | 'PROTECTED' | 'PRIVATE' | 'INTERNAL';

// Table 40 – Function block type declaration
// Table 41 - Function block instance declaration
fb_type_name : std_fb_name | derived_fb_name;
fb_type_access : ( namespace_name '.' )* fb_type_name;
std_fb_name : 'SR' | 'RS' | 'R_TRIG' | 'F_TRIG' | 'CTU'| 'CTD' | 'CTUD' | 'TP' | 'TON' | 'TOF';         // incomplete list
derived_fb_name : ID;
fb_decl     : 'FUNCTION_BLOCK' ( 'FINAL' | 'ABSTRACT' )? derived_fb_name using_directive *
            ( 'EXTENDS' ( fb_type_access | class_type_access ) )?
            ( 'IMPLEMENTS' interface_name_list )?
            ( fb_io_var_decls | func_var_decls | temp_var_decls | other_var_decls )*
            ( method_decl )* fb_body ? 'END_FUNCTION_BLOCK';
fb_io_var_decls : fb_input_decls | fb_output_decls | in_out_decls;
fb_input_decls : 'VAR_INPUT' ( 'RETAIN' | 'NON_RETAIN' )? ( fb_input_decl ';' )* 'END_VAR';
fb_input_decl : var_decl_init | edge_decl | array_conform_decl;
fb_output_decls : 'VAR_OUTPUT' ( 'RETAIN' | 'NON_RETAIN' )? ( fb_output_decl ';' )* 'END_VAR';
fb_output_decl : var_decl_init | array_conform_decl;
other_var_decls : retain_var_decls | no_retain_var_decls | loc_partly_var_decl;
no_retain_var_decls : 'VAR' 'NON_RETAIN' access_spec ? ( var_decl_init ';' )* 'END_VAR';
//FB_Body : sfc | ladder_diagram | fb_diagram | instruction_List | stmt_list | other_languages;
fb_body : sfc | instruction_list | stmt_list ;
method_decl     : 'METHOD' access_spec ( 'FINAL' | 'ABSTRACT' )? 'OVERRIDE' ?
                method_name ( ':' data_type_access )?
                ( io_var_decls | func_var_decls | temp_var_decls )* func_body 'END_METHOD';
method_name : ID;

// Table 19 - Function declaration
func_name : std_func_name | derived_func_name;
func_access : ( namespace_name '.' )* func_name;
std_func_name   : 'TRUNC' | 'ABS' | 'SQRT' | 'LN' | 'LOG' | 'EXP'
                | 'SIN' | 'COS' | 'TAN' | 'ASIN' | 'ACOS' | 'ATAN' | 'ATAN2 '
                | 'ADD' | 'SUB' | 'MUL' | 'DIV' | 'MOD' | 'EXPT' | 'MOVE '
                | 'SHL' | 'SHR' | 'ROL' | 'ROR'
                | 'AND' | 'OR' | 'XOR' | 'NOT'
                | 'SEL' | 'MAX' | 'MIN' | 'LIMIT' | 'MUX '
                | 'GT' | 'GE' | 'EQ' | 'LE' | 'LT' | 'NE'
                | 'LEN' | 'LEFT' | 'RIGHT' | 'MID' | 'CONCAT' | 'INSERT' | 'DELETE' | 'REPLACE' | 'FIND';       // incomplete list
derived_func_name : ID;
func_decl   : 'FUNCTION' derived_func_name ( ':' data_type_access )? using_directive *
            ( io_var_decls | func_var_decls | temp_var_decls )* func_body 'END_FUNCTION';
io_var_decls : input_decls | output_decls | in_out_decls;
func_var_decls : external_var_decls | var_decls;
//func_body : ladder_diagram | fb_diagram | instruction_list | stmt_list | other_languages;
func_body : instruction_list | stmt_list;

// Table 64 - Namespace
namespace_decl  : 'NAMESPACE' 'INTERNAL' ? namespace_h_name using_directive * namespace_elements
                'END_NAMESPACE';
namespace_elements  : ( data_type_decl | func_decl | fb_decl
                    | class_decl | interface_decl | namespace_decl )+;
namespace_h_name : namespace_name ( '.' namespace_name )*;
namespace_name : ID;
using_directive : 'USING' namespace_h_name ( ',' namespace_h_name )* ';';
pou_decl    : using_directive *
            ( global_var_decls | data_type_decl | access_decls
            | func_decl | fb_decl | class_decl | interface_decl
            | namespace_decl )+;

// Table 13 - Declaration of variables/Table 14 – Initialization of variables
variable : DIRECT_VARIABLE | symbolic_variable;
symbolic_variable : ( ( 'THIS' '.' ) | ( namespace_name '.' )+ )? ( var_access | multi_elem_var );
var_access : variable_name | ref_deref;
variable_name : ID;
multi_elem_var : var_access ( subscript_list | struct_variable )+;
subscript_list : '[' subscript ( ',' subscript )* ']';
subscript : expression;
struct_variable : '.' struct_elem_select;
struct_elem_select : var_access;
input_decls : 'VAR_INPUT' ( 'RETAIN' | 'NON_RETAIN' )? ( input_decl ';' )* 'END_VAR';
input_decl : var_decl_init | edge_decl | array_conform_decl;
edge_decl : variable_list ':' 'BOOL' ( 'R_EDGE' | 'F_EDGE' );
//var_decl_init   : variable_list ':' ( simple_spec_init | str_var_decl | ref_spec_init )
//                | array_var_decl_init | struct_var_decl_init | FB_Decl_Init | Interface_Spec_Init;
var_decl_init   : variable_list ':' ( simple_spec_init | str_var_decl | ref_spec_init )
                | array_var_decl_init | struct_var_decl_init;
ref_var_decl : variable_list ':' ref_spec;
interface_var_decl : variable_list ':' interface_type_access;
variable_list : variable_name ( ',' variable_name )*;
array_var_decl_init : variable_list ':' array_spec_init;
array_conformand : 'ARRAY' '[' '*' ( ',' '*' )* ']' 'OF' data_type_access;
array_conform_decl : variable_list ':' array_conformand;
struct_var_decl_init : variable_list ':' struct_spec_init;
fb_decl_no_init : fb_name ( ',' fb_name )* ':' fb_type_access;
fb_decl_init : fb_decl_no_init ( ':=' struct_init )?;
fb_name : ID;
fb_instance_name : ( namespace_name '.' )* fb_name '^' *;
output_decls : 'VAR_OUTPUT' ( 'RETAIN' | 'NON_RETAIN' )? ( output_decl ';' )* 'END_VAR';
output_decl : var_decl_init | array_conform_decl;
in_out_decls : 'VAR_IN_OUT' ( in_out_var_decl ';' )* 'END_VAR';
in_out_var_decl : var_decl | array_conform_decl | fb_decl_no_init;
var_decl : variable_list ':' ( simple_spec | str_var_decl | array_var_decl | struct_var_decl );
array_var_decl : variable_list ':' array_spec;
struct_var_decl : variable_list ':' struct_type_access;
var_decls : 'VAR' 'constant' ? access_spec ? ( var_decl_init ';' )* 'END_VAR';
retain_var_decls : 'VAR' 'RETAIN' access_spec ? ( var_decl_init ';' )* 'END_VAR';
loc_var_decls : 'VAR' ( 'constant' | 'RETAIN' | 'NON_RETAIN' )? ( loc_var_decl ';' )* 'END_VAR';
loc_var_decl : variable_name ? located_at ':' loc_var_spec_init;
temp_var_decls : 'VAR_TEMP' ( ( var_decl | ref_var_decl | interface_var_decl ) ';' )* 'END_VAR';
external_var_decls : 'VAR_EXTERNAL' 'constant' ? ( external_decl ';' )* 'END_VAR';
external_decl   : global_var_name ':'
                ( simple_spec | array_spec | struct_type_access | fb_type_access | ref_type_access );
global_var_name : ID;
global_var_decls : 'VAR_GLOBAL' ( 'constant' | 'RETAIN' )? ( global_var_decl ';' )* 'END_VAR';
global_var_decl : global_var_spec ':' ( loc_var_spec_init | fb_type_access );
global_var_spec : ( global_var_name ( ',' global_var_name )* ) | ( global_var_name located_at );
loc_var_spec_init : simple_spec_init | array_spec_init | struct_spec_init | s_byte_str_spec | d_byte_str_spec;
located_at      : 'AT' DIRECT_VARIABLE;
str_var_decl    : s_byte_str_var_decl | d_byte_str_var_decl;
s_byte_str_var_decl : variable_list ':' s_byte_str_spec;
s_byte_str_spec : 'STRING' ( '[' UNSIGNED_INT ']' )? ( ':=' S_BYTE_CHAR_STR )?;
d_byte_str_var_decl : variable_list ':' d_byte_str_spec;
d_byte_str_spec : 'WSTRING' ( '[' UNSIGNED_INT ']' )? ( ':=' D_BYTE_CHAR_STR )?;
loc_partly_var_decl : 'VAR' ( 'RETAIN' | 'NON_RETAIN' )? loc_partly_var * 'END_VAR';
loc_partly_var  : variable_name 'AT' '%' ( 'I' | 'Q' | 'M' ) '*' ':' var_spec ';';
var_spec    : simple_spec | array_spec | struct_type_access
            | ( 'STRING' | 'WSTRING' ) ( '[' UNSIGNED_INT ']' )?;

// Table 12 - Reference operations
ref_type_decl : ref_type_name ':' ref_spec_init;
ref_spec_init : ref_spec ( ':=' ref_value )?;
ref_spec : 'REF_TO' + data_type_access;
ref_type_name : ID;
ref_type_access : ( namespace_name '.' )* ref_type_name;
ref_name : ID;
ref_value : ref_addr | 'NULL';
ref_addr : 'REF' '(' ( symbolic_variable | fb_instance_name | class_instance_name ) ')';
ref_assign : ref_name ':=' ( ref_name | ref_deref | ref_value );
ref_deref : ref_name '^' +;

//PARSER RULES
//Table 10
data_type_access    : ELEM_TYPE_NAME | derived_type_access;

// Table 11 - Declaration of user-defined data types and initialization
data_type_decl  : 'TYPE' ( type_decl ';' )+ 'END_TYPE';
type_decl       : simple_type_decl | subrange_type_decl | enum_type_decl
                | array_type_decl | struct_type_decl
                | str_type_decl | ref_type_decl;
simple_type_decl : simple_type_name ':' simple_spec_init;
simple_spec_init : simple_spec ( ':=' constant_expr )?;
simple_spec     : ELEM_TYPE_NAME | simple_type_access;
subrange_type_decl : subrange_type_name ':' subrange_spec_init;
subrange_spec_init : subrange_spec ( ':=' signed_int )?;
subrange_spec : INT_TYPE_NAME '(' subrange ')' | subrange_type_access;
subrange : constant_expr '..' constant_expr;
enum_type_decl : enum_type_name ':' ( ( ELEM_TYPE_NAME ? named_spec_init ) | enum_spec_init );
named_spec_init : '(' enum_value_spec ( ',' enum_value_spec )* ')' ( ':=' enum_value )?;
enum_spec_init : ( ( '(' ID ( ',' ID )* ')' ) | enum_type_access ) ( ':=' enum_value )?;
enum_value_spec : ID ( ':=' ( int_literal | constant_expr ) )?;
enum_value : ( enum_type_name '#' )? ID;
array_type_decl : array_type_name ':' array_spec_init;
array_spec_init : array_spec ( ':=' array_init )?;
array_spec : array_type_access | 'ARRAY' '[' subrange ( ',' subrange )* ']' 'OF' data_type_access;
array_init : '[' array_elem_init ( ',' array_elem_init )* ']';
array_elem_init : array_elem_init_value | UNSIGNED_INT '(' array_elem_init_value ? ')';
array_elem_init_value : constant_expr | enum_value | struct_init | array_init;
struct_type_decl : struct_type_name ':' struct_spec;
struct_spec : struct_decl | struct_spec_init;
struct_spec_init : struct_type_access ( ':=' struct_init )?;
struct_decl :'STRUCT' 'OVERLAP' ? ( struct_elem_decl ';' )+ 'END_STRUCT';
struct_elem_decl : struct_elem_name ( located_at MULTIBIT_PART_ACCESS ? )? ':'
                ( simple_spec_init | subrange_spec_init | enum_spec_init | array_spec_init | struct_spec_init );
struct_elem_name : ID;
struct_init : '(' struct_elem_init ( ',' struct_elem_init )* ')';
struct_elem_init : struct_elem_name ':=' ( constant_expr | enum_value | array_init | struct_init | ref_value );
str_type_decl : STRING_TYPE_NAME ':' STRING_TYPE_NAME ( ':=' CHAR_STR )?;

derived_type_access : single_elem_type_access | array_type_access | struct_type_access
                    | string_type_access | class_type_access | ref_type_access | interface_type_access;
single_elem_type_access     : simple_type_access | subrange_type_access | enum_type_access;
array_type_access   : ( namespace_name '.' )* array_type_name;
struct_type_access  : ( namespace_name '.' )* struct_type_name;
string_type_access  : ( namespace_name '.' )* STRING_TYPE_NAME;

simple_type_access      : ( namespace_name '.' )* simple_type_name;
subrange_type_access    : ( namespace_name '.' )* subrange_type_name;
enum_type_access        : ( namespace_name '.' )* enum_type_name;
array_type_name     : ID;
struct_type_name    : ID;
simple_type_name    : ID;
subrange_type_name  : ID;
enum_type_name      : ID;

// Table 5 - Numeric literal
constant    : numeric_literal | CHAR_LITERAL | TIME_LITERAL | BIT_STR_LITERAL | BOOL_LITERAL;
numeric_literal : int_literal | real_literal;
int_literal     : ( INT_TYPE_NAME '#' )? ( signed_int | BINARY_INT | OCTAL_INT | HEX_INT );
real_literal    : ( REAL_TYPE_NAME '#' )? signed_int '.' UNSIGNED_INT ( 'E' signed_int )?;
signed_int      : ( '+' | '-' )? UNSIGNED_INT;



/*********************************************************************************************************************************************** 
LEXER RULES 
************************************************************************************************************************************************/

// Table 71 - 72 - Language Structured Text (ST)
MULTIBIT_PART_ACCESS : '.' ( UNSIGNED_INT | '%' ( 'X' | 'B' | 'W' | 'D' | 'L' ) ? UNSIGNED_INT );

// Table 67 - 70 - Instruction List (IL)
IL_SIMPLE_OPERATOR  : 'LD' | 'LDN' | 'ST' | 'STN' | 'ST?' | 'NOT' | 'S' | 'R'
                    | 'S1' | 'R1' | 'CLK' | 'CU' | 'CD' | 'PV'
                    | 'IN' | 'PT' | IL_EXPR_OPERATOR;
IL_EXPR_OPERATOR    : 'AND' | '&' | 'OR' | 'XOR' | 'ANDN' | '&N' | 'ORN'
                    | 'XORN' | 'ADD' | 'SUB' | 'MUL' | 'DIV'
                    | 'MOD' | 'GT' | 'GE' | 'EQ' | 'LT' | 'LE' | 'NE';
IL_CALL_OPERATOR    : 'CAL' | 'CALC' | 'CALCN';
IL_RETURN_OPERATOR  : 'RT' | 'RETC' | 'RETCN';
IL_JUMP_OPERATOR    : 'JMP' | 'JMPC' | 'JMPCN';

// Table 16 - Directly represented variables
DIRECT_VARIABLE : '%' ( 'I' | 'Q' | 'M' ) ( 'X' | 'B' | 'W' | 'D' | 'L' )? UNSIGNED_INT ( '.' UNSIGNED_INT )*;

// Table 8 - Duration literals
// Table 9 – Date and time of day literals
TIME_LITERAL    : DURATION | TIME_OF_DAY | DATE | DATE_AND_TIME;
DURATION        : ( TIME_TYPE_NAME | 'T' | 'LT' ) '#' ( '+' | '-' )? INTERVAL;
TIME_OF_DAY     : ( TOD_TYPE_NAME | 'LTIME_OF_DAY' ) '#' DAYTIME;
DATE            : ( DATE_TYPE_NAME | 'D' | 'LD' ) '#' DATE_LITERAL;
DATE_AND_TIME   : ( DT_TYPE_NAME | 'LDATE_AND_TIME' ) '#' DATE_LITERAL '-' DAYTIME;
INTERVAL        : DAYS | HOURS | MINUTES | SECONDS | MILLISECONDS | MICROSECONDS | NANOSECONDS;
DAYTIME         : DAY_HOUR ':' DAY_MINUTE ':' DAY_SECOND;
DATE_LITERAL    : YEAR '-' MONTH '-' DAY;
fragment DAYS   : ( FIX_POINT 'd' ) | ( UNSIGNED_INT 'd' '_'?  HOURS?);
fragment HOURS  : ( FIX_POINT 'h' ) | ( UNSIGNED_INT 'h' '_'?  MINUTES?);
fragment MINUTES    : ( FIX_POINT 'm' ) | ( UNSIGNED_INT 'm' '_'?  SECONDS?);
fragment SECONDS    : ( FIX_POINT 's' ) | ( UNSIGNED_INT 's' '_'?  MILLISECONDS?);
fragment MILLISECONDS   : ( FIX_POINT 'ms' ) | ( UNSIGNED_INT 'ms' '_'?  MICROSECONDS?);
fragment MICROSECONDS   : ( FIX_POINT 'us' ) | ( UNSIGNED_INT 'us' '_'?  NANOSECONDS?);
fragment NANOSECONDS    : FIX_POINT 'ns';
fragment DAY_HOUR   : UNSIGNED_INT;
fragment DAY_MINUTE : UNSIGNED_INT;
fragment DAY_SECOND : FIX_POINT;
fragment YEAR   : UNSIGNED_INT;
fragment MONTH  : UNSIGNED_INT;
fragment DAY    : UNSIGNED_INT;
fragment FIX_POINT : UNSIGNED_INT ( '.' UNSIGNED_INT )?;

// Table 6 - Character String literals
// Table 7 - Two-character combinations in character strings
CHAR_LITERAL        : ( 'STRING#' )? CHAR_STR;
CHAR_STR : S_BYTE_CHAR_STR | D_BYTE_CHAR_STR;
S_BYTE_CHAR_STR : '\'' S_BYTE_CHAR_VALUE + '\'';
D_BYTE_CHAR_STR : '"' D_BYTE_CHAR_VALUE + '"';
S_BYTE_CHAR_VALUE : COMMON_CHAR_VALUE | '$\'' | '"' | '$' HEX_DIGIT HEX_DIGIT;
D_BYTE_CHAR_VALUE : COMMON_CHAR_VALUE | '\'' | '$"' | '$' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT;
fragment COMMON_CHAR_VALUE   : '!' | '#' | '%' | '&' | '('..'/' | '0'..'9' | ':'..'@' | 'A'..'Z' | '['..'`' | 'a'..'z' | '{'..'~'
                             | '$$' | '$L' | '$N' | '$P' | '$R' | '$T';


// Table 5 - Numeric literal


BOOL_LITERAL    : ( BOOL_TYPE_NAME '#' )? ( '0' | '1' | 'FALSE' | 'TRUE' );
BIT_STR_LITERAL : ( MULTIBITS_TYPE_NAME '#' )? ( UNSIGNED_INT | BINARY_INT | OCTAL_INT | HEX_INT );

HEX_INT         : '16#' ( '_' ? HEX_DIGIT )+;
OCTAL_INT       : '8#' ( '_' ? OCTAL_DIGIT )+;
BINARY_INT     : '2#' ( '_' ? BIT )+;

UNSIGNED_INT    : DIGIT ( '_' ? DIGIT )*;

// Table 10 - Elementary data types
//Data_Type_Access    : Elem_Type_Name | Derived_Type_Access;     //define as parser rule
ELEM_TYPE_NAME      : NUMERIC_TYPE_NAME | BIT_STR_TYPE_NAME
                    | STRING_TYPE_NAME  | DATE_TYPE_NAME | TIME_TYPE_NAME;
NUMERIC_TYPE_NAME   : INT_TYPE_NAME | REAL_TYPE_NAME;
BIT_STR_TYPE_NAME   : BOOL_TYPE_NAME | MULTIBITS_TYPE_NAME;
STRING_TYPE_NAME    : 'STRING' ( '[' UNSIGNED_INT ']' )? | 'WSTRING' ( '[' UNSIGNED_INT ']' )? | 'CHAR' | 'WCHAR';
DATE_TYPE_NAME      : 'DATE' | 'LDATE';
TIME_TYPE_NAME      : 'TIME' | 'LTIME';
TOD_TYPE_NAME       : 'TIME_OF_DAY' | 'TOD' | 'LTOD';
DT_TYPE_NAME        : 'DATE_AND_TIME' | 'DT' | 'LDT';
INT_TYPE_NAME       : SIGN_INT_TYPE_NAME | UNSIGN_INT_TYPE_NAME;
REAL_TYPE_NAME      : 'REAL' | 'LREAL';
BOOL_TYPE_NAME      : 'BOOL';
MULTIBITS_TYPE_NAME : 'BYTE' | 'WORD' | 'DWORD' | 'LWORD';
SIGN_INT_TYPE_NAME  : 'SINT' | 'INT' | 'DINT' | 'LINT';
UNSIGN_INT_TYPE_NAME    : 'USINT' | 'UINT' | 'UDINT' | 'ULINT';

// Table 4 - Pragma
PRAGMA : '{' .*? '}' -> channel(1);  

//Tabla 3
EOL : [\r\n]+ ;                                             //End of Line token
WS  : [ \t\r\n]+ -> channel(1) ;                       //White space token
LINE_COMMENT    : '//' .*? '\r' ? '\n' -> channel(1) ;
COMMENT_LEGACY  : '(*' .*? '*)' -> channel(1) ;                  //Comment tokent
COMMENT         : '/*' .*? '*/' -> channel(1) ;  

//Tabla 1
ID : LETTER ( LETTER | DIGIT )*;                    //Token for identifiers
fragment HEX_DIGIT  : [0-9a-fA-F];                          //token for hexadecimals number. Case insensitive
fragment OCTAL_DIGIT    : [0-7];
fragment BIT        : [0-1];
fragment DIGIT      : [0-9];
fragment LETTER     : [a-zA-Z_] ;
