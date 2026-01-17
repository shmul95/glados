somewhere
{
    extern def CreateObject(mass: f32, position: Vec2, name: string, flag: char) -> Object;
    extern def UpdateObject(obj: *Object, deltaMass: f32, deltaPos: Vec2) -> null;
    extern def GetObjectInfo(obj: Object, mass: *f32, position: *Vec2, name: *string, flag: *char) -> null;

    def assert(condition: bool, message: string) -> bool;
}

/**
 * data structures
 */

struct Vec2
{
    x: f32;
    y: f32;
}

/**
 * memory layout designed to match a 64-bit C structure
 * TOTAL SIZE: 32 bytes
 */
struct Object
{
    mass:     f32;      // 4 bytes
    position: Vec2;     // 8 bytes (ACCUMULATED: 12 bytes)
    _pad:     u32;      // 4 bytes padding to align the pointer (ACCUMULATED: 16 bytes)
    name:     string;   // 8 bytes (POINTER)
    flag:     char;     // 1 byte
}

/**
* public
*/

export def main() -> null
{
    start_pos = Vec2 { x: 10.0, y: 20.0 };
    expected_name = "RuneEntity";
    
    obj = CreateObject(1.5: f32, start_pos, expected_name, 'R');

    assert(obj.mass == 1.5: f32, "CreateObject: mass");
    assert(obj.position.x == 10.0: f32, "CreateObject: X coordinate");
    assert(obj.position.y == 20.0: f32, "CreateObject: Y coordinate");
    assert(obj.flag == 'R', "CreateObject: flag");

    delta_pos = Vec2 { x: 5.5: f32, y: -2.5: f32 };
    UpdateObject(&obj, 0.5: f32, delta_pos);

    assert(obj.mass == 2.0: f32, "UpdateObject: Mass was incremented");
    assert(obj.position.x == 15.5: f32, "UpdateObject: X was incremented");
    assert(obj.position.y == 17.5: f32, "UpdateObject: Y was decremented");
}
