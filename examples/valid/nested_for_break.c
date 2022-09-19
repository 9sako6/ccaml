int main() {
    int i = 0;
    int j = 0;
    int sum = 0;
    for (i = 0; i < 10; i = i + 1) {
        for (j = 0; j < 10; j = j + 1) {
            sum = sum - j;
            if (j > 4) break;
        }
        sum = sum + i;
    }

    return sum;
}
