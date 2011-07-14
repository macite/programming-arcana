/* program: linked-list.c */
#include <stdlib.h>
#include <stdio.h>


// =====================
// = Type Declarations =
// =====================

typedef struct node_struct *node_ptr;

typedef struct node_struct
{
    int         data;
    node_ptr    next;
} node;



// ========================================
// = Linked List Functions and Procedures =
// ========================================

// Creates a new node on the heap
node *create_node(int value, node *next)
{
    node *result = NULL;
    result = calloc(1, sizeof(node));
    
    if (result != NULL)
    {
        result->data = value;
        result->next = next;
    }
    
    return result;
}

void print_node(node *n)
{
    if (n != NULL)
    {
        printf("%d -> ", n->data);
    }
    else
    {
        printf("NULL");
    }
}

node *node_after(node *current)
{
    if (current != NULL)
        return current->next;
    else
        return NULL;
}

node *node_before(node *current, node *start)
{
    node *temp = start;
    
    while ( temp != NULL )
    {
        if ( node_after(temp) == current) 
            return temp;
        
        temp = node_after(temp);
    }
    
    return NULL;
}

void print_list(node *start)
{
    node *current = start;
    printf("-> ");
    
    while ( current != NULL )
    {
        print_node(current);
        current = node_after(current);
    }
    
    printf("END\n");
}

node *find_first_node_with_value(node *start, int value)
{
    node *current = start;
    
    while ( current != NULL )
    {
        if ( current->data == value ) return current;
        current = node_after(current);
    }
    
    return NULL;
}

void delete_node(node *n, node **start)
{
    node *current = n;
    node *next = node_after(n);
    node *prev = node_before(n, *start);
    
    if ( prev == NULL )
    {
        // Node is either not in this list, or the first element
        if (n == *start)
        {
            // Deleting the first node, so change the start
            *start = next;
        }
        else
        {
            // Node was just not in the list... this is an error... so exit.
            return;
        }
    }
    else
    {
        // Skip the node that is about to be deleted
        prev->next = next;
    }
    
    if(current != NULL)
    {
        free(current);
    }
}

void free_list(node **start)
{
    if (start == NULL) return;
    
    node *current = *start;
    node *next = NULL;
    
    while(current != NULL)
    {
        next = node_after(current); // must get this before disposing...
        free(current);
        current = next;
    }
    
    *start = NULL;
}



// ========
// = Main =
// ========

int main()
{
    node *start = NULL;
    int count, i;
    
    printf("How many nodes do you want to add: ");
    scanf("%d", &count);
    
    for(i = 0; i < count; i++)
    {
        start = create_node(i, start);
    }
        
    print_list(start);
    
    printf("The start is... ");
    print_node(start);
    printf("\n");
    
    printf("The node after start is... ");
    print_node(node_after(start));
    printf("\n");
    
    printf("The node before start is... ");
    print_node(node_before(start, start));
    printf("\n");
    
    printf("The node before the node with value 1 is... ");
    print_node(node_before(find_first_node_with_value(start, 1), start));
    printf("\n");
    
    printf("Deleting node with value 3 list is now... ");
    delete_node(find_first_node_with_value(start, 3), &start);
    print_list(start);
    
    printf("Deleting the first node from the list. It is now... ");
    delete_node(start, &start);
    print_list(start);
    
    free_list(&start);
    start = NULL;
    
    return 0;
}

