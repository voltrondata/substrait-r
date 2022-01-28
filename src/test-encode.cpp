#include <cpp11.hpp>
using namespace cpp11;

#include "pb_encode.h"
#include "pb_decode.h"
#include "substrait/type.pb.h"

class Encoder {
public:
    pb_ostream_t stream;

    Encoder(size_t max_size = 2147483647) {
        stream.max_size = max_size;
        stream.bytes_written = 0;
        stream.state = this;
        stream.errmsg = NULL;
        stream.callback = &encoder_callback;

        memset(error_message, 0, 1024);
    }

protected:
    virtual void write(const uint8_t* buf, size_t count) {
        throw std::runtime_error("write() not implemented");
    }

private:
    char error_message[1024];

    static bool encoder_callback(pb_ostream_t* stream, const uint8_t* buf, size_t count) {
        Encoder* encoder = reinterpret_cast<Encoder*>(stream->state);
        try {
            encoder->write(buf, count);
            return true;
        } catch (std::exception& e) {
            const char* err = e.what();
            int n_cpy = std::min<int>(1024 - 1, strlen(err));
            memcpy(encoder->error_message, err, n_cpy);
            encoder->error_message[n_cpy] = '\0';
            return false;
        }
    }

};

class RawEncoder: public Encoder {
public:
    RawEncoder(): vec((R_xlen_t) 0) {}

    cpp11::raws result() {
        return vec;
    }

protected:

    void write(const uint8_t* buf, size_t count) {
        vec.reserve(vec.size() + count);
        for (size_t i = 0; i < count; i++) {
            vec.push_back(buf[i]);
        }
    }

private:
    writable::raws vec;

};


void encode_substrait_Type_Boolean(Encoder& encoder,
                                   uint32_t type_variation_reference,
                                   substrait_Type_Nullability nullablity) {

    substrait_Type_Boolean value = substrait_Type_Boolean_init_zero;
    value.type_variation_reference = &type_variation_reference;
    value.nullability = &nullablity;

    if (!pb_encode(&encoder.stream, substrait_Type_Boolean_fields, &value)) {
        cpp11::stop("Encoding error: %s", PB_GET_ERROR(&encoder.stream));
    }
}

[[cpp11::register]]
raws r_encode_substrait_Type_Boolean(int type_variation_reference,
                                     int nullablity) {
    RawEncoder encoder;
    encode_substrait_Type_Boolean(encoder, type_variation_reference, (substrait_Type_Nullability) nullablity);
    return encoder.result();
}
