somewhere
{
    /* window */
    extern def InitWindow(width: i32, height: i32, title: string) -> null;
    extern def CloseWindow() -> null;
    extern def WindowShouldClose() -> bool;

    /* drawing */
    extern def BeginDrawing() -> null;
    extern def EndDrawing() -> null;
    extern def ClearBackground(color: Color) -> null;
    extern def DrawText(text: string, x: i32, y: i32, font_size: i32, color: Color) -> null;

    /* utils */
    extern def SetTargetFPS(fps: i32) -> null;
}

/**
* data types
*/

struct Color
{
    r: u8;
    g: u8;
    b: u8;
    a: u8;
}

/**
* public
*/

def main() -> null
{
    screen_width    = 800;
    screen_height   = 450;

    COLOR_WHITE     = Color { r: 255, g: 255, b: 255, a: 255 };
    COLOR_LIGHTGRAY = Color { r: 200, g: 200, b: 200, a: 255 };

    InitWindow(screen_width, screen_height, "RuneLang Window!");
    SetTargetFPS(60);

    loop
    {
        if WindowShouldClose()
        {
            stop;
        }

        BeginDrawing();
        ClearBackground(COLOR_WHITE);
        DrawText("Hello, RuneLang!", 190, 200, 20, COLOR_LIGHTGRAY);
        EndDrawing();

    }

    CloseWindow();
}
