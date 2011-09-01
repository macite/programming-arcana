/* struct example */
#include <stdlib.h>

typedef struct node_struct
{
    int                 value; // node value
    struct node_struct  *next; // pointer to next node
} node;

node *create_node(int val, node *next)
{
    node *result;
    result = (node *)malloc(sizeof(node));
    result->value = val;
    result->next = next;
    return result;
}

int main()
{
    node *current;
    current = create_node(0, NULL);
    current = create_node(1, current);
    return 0;
}