// tokeniser.h : shared definition for tokeniser.l and compilateur.cpp

// On declare un type enumere TOKEN
enum TOKEN
{
    FEOF,        // Fin de fichier
    UNKNOWN,     // Inconnu
    NUMBER,      // Nombre et pas un chiffre
    ID,          // Identificateur (une lettre suivi de lettres ou chiffres)
    STRINGCONST, // Chaine de caracteres
    CHARCONST,   // Caractere
    RBRACKET,    // ]
    LBRACKET,    // [
    RPARENT,     // (
    LPARENT,     // )
    COMMA,       // ,
    SEMICOLON,   // ;
    COLON,       // :
    DOT,         // .
    ADDOP,       // + ou -
    MULOP,       // x ou /
    RELOP,       // <, >, <=, >=, =, <>
    NOT,         // not
    ASSIGN,      // :=
    KEYWORD,     // mot-cle

};
