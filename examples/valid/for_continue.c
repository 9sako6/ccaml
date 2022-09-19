int main() {
    int i;
    int j = 0;
    for (i = 0; i < 100; i = i + 1) {
        if (i > 10) continue;
        j = j + i;
    }

    return j;
}
