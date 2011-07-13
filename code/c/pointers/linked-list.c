/* program: linked-list.c */

typedef struct
{
    int     size;
    char    *text;
} text_data;

typedef union
{
    int         int_value;
    double      dbl_value;
    text_data   txt_value;
} node_data;

typedef struct node_struct
{
    node_data           data;
    struct node_struct  *next;
} node_data, *node;

int main()
{
    node first;
    return 0;
}