typedef struct Vec2
{
    float x;
    float y;
} Vec2;

typedef struct Object
{
    float mass;
    Vec2 position;
    char *name;
    char flag;
} Object;


Object CreateObject(float mass, Vec2 position, char *name, char flag)
{
    Object obj;
    obj.mass = mass;
    obj.position = position;
    obj.name = name;
    obj.flag = flag;
    return obj;
}


void UpdateObject(Object *obj, float deltaMass, Vec2 deltaPosition)
{
    obj->mass += deltaMass;
    obj->position.x += deltaPosition.x;
    obj->position.y += deltaPosition.y;
}


void GetObjectInfo(Object obj, float *mass, Vec2 *position, char **name, char *flag)
{
    *mass = obj.mass;
    *position = obj.position;
    *name = obj.name;
    *flag = obj.flag;
}

